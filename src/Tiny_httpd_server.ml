type buf = Tiny_httpd_buf.t
type byte_stream = Tiny_httpd_stream.t

let _debug_on =
  ref
    (match String.trim @@ Sys.getenv "HTTP_DBG" with
    | "" -> false
    | _ -> true
    | exception _ -> false)

let _enable_debug b = _debug_on := b

let _debug k =
  if !_debug_on then
    k (fun fmt ->
        Printf.fprintf stdout "[http.thread %d]: " Thread.(id @@ self ());
        Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") stdout fmt)

module Buf = Tiny_httpd_buf
module Byte_stream = Tiny_httpd_stream
module IO = Tiny_httpd_io
module Pool = Tiny_httpd_pool

exception Bad_req of int * string

let bad_reqf c fmt = Printf.ksprintf (fun s -> raise (Bad_req (c, s))) fmt

module Response_code = struct
  type t = int

  let ok = 200
  let not_found = 404

  let descr = function
    | 100 -> "Continue"
    | 200 -> "OK"
    | 201 -> "Created"
    | 202 -> "Accepted"
    | 204 -> "No content"
    | 300 -> "Multiple choices"
    | 301 -> "Moved permanently"
    | 302 -> "Found"
    | 304 -> "Not Modified"
    | 400 -> "Bad request"
    | 401 -> "Unauthorized"
    | 403 -> "Forbidden"
    | 404 -> "Not found"
    | 405 -> "Method not allowed"
    | 408 -> "Request timeout"
    | 409 -> "Conflict"
    | 410 -> "Gone"
    | 411 -> "Length required"
    | 413 -> "Payload too large"
    | 417 -> "Expectation failed"
    | 500 -> "Internal server error"
    | 501 -> "Not implemented"
    | 503 -> "Service unavailable"
    | n -> "Unknown response code " ^ string_of_int n (* TODO *)
end

type 'a resp_result = ('a, Response_code.t * string) result

let unwrap_resp_result = function
  | Ok x -> x
  | Error (c, s) -> raise (Bad_req (c, s))

module Meth = struct
  type t = [ `GET | `PUT | `POST | `HEAD | `DELETE | `OPTIONS ]

  let to_string = function
    | `GET -> "GET"
    | `PUT -> "PUT"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `DELETE -> "DELETE"
    | `OPTIONS -> "OPTIONS"

  let pp out s = Format.pp_print_string out (to_string s)

  let of_string = function
    | "GET" -> `GET
    | "PUT" -> `PUT
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | "DELETE" -> `DELETE
    | "OPTIONS" -> `OPTIONS
    | s -> bad_reqf 400 "unknown method %S" s
end

module Headers = struct
  type t = (string * string) list

  let empty = []

  let contains name headers =
    let name' = String.lowercase_ascii name in
    List.exists (fun (n, _) -> name' = n) headers

  let get_exn ?(f = fun x -> x) x h =
    let x' = String.lowercase_ascii x in
    List.assoc x' h |> f

  let get ?(f = fun x -> x) x h =
    try Some (get_exn ~f x h) with Not_found -> None

  let remove x h =
    let x' = String.lowercase_ascii x in
    List.filter (fun (k, _) -> k <> x') h

  let set x y h =
    let x' = String.lowercase_ascii x in
    (x', y) :: List.filter (fun (k, _) -> k <> x') h

  let pp out l =
    let pp_pair out (k, v) = Format.fprintf out "@[<h>%s: %s@]" k v in
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l

  (* token = 1*tchar
     tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
              / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
     Reference: https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 *)
  let is_tchar = function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
      true
    | _ -> false

  let for_all pred s =
    try
      String.iter (fun c -> if not (pred c) then raise Exit) s;
      true
    with Exit -> false

  let parse_ ~buf (bs : byte_stream) : t =
    let rec loop acc =
      let line = Byte_stream.read_line ~buf bs in
      _debug (fun k -> k "parsed header line %S" line);
      if line = "\r" then
        acc
      else (
        let k, v =
          try
            let i = String.index line ':' in
            let k = String.sub line 0 i in
            if not (for_all is_tchar k) then
              invalid_arg (Printf.sprintf "Invalid header key: %S" k);
            let v =
              String.sub line (i + 1) (String.length line - i - 1)
              |> String.trim
            in
            k, v
          with _ -> bad_reqf 400 "invalid header line: %S" line
        in
        loop ((String.lowercase_ascii k, v) :: acc)
      )
    in
    loop []
end

module Request = struct
  type 'body t = {
    meth: Meth.t;
    host: string;
    client_addr: Unix.sockaddr option;
    headers: Headers.t;
    http_version: int * int;
    path: string;
    path_components: string list;
    query: (string * string) list;
    body: 'body;
    start_time: float;
  }

  let headers self = self.headers
  let host self = self.host
  let meth self = self.meth
  let path self = self.path
  let body self = self.body
  let start_time self = self.start_time
  let query self = self.query
  let get_header ?f self h = Headers.get ?f h self.headers

  let get_header_int self h =
    match get_header self h with
    | Some x -> (try Some (int_of_string x) with _ -> None)
    | None -> None

  let set_header k v self = { self with headers = Headers.set k v self.headers }
  let update_headers f self = { self with headers = f self.headers }
  let set_body b self = { self with body = b }

  (** Should we close the connection after this request? *)
  let close_after_req (self : _ t) : bool =
    match self.http_version with
    | 1, 1 -> get_header self "connection" = Some "close"
    | 1, 0 -> not (get_header self "connection" = Some "keep-alive")
    | _ -> false

  let pp_comp_ out comp =
    Format.fprintf out "[%s]"
      (String.concat ";" @@ List.map (Printf.sprintf "%S") comp)

  let pp_query out q =
    Format.fprintf out "[%s]"
      (String.concat ";"
      @@ List.map (fun (a, b) -> Printf.sprintf "%S,%S" a b) q)

  let pp_ out self : unit =
    Format.fprintf out
      "{@[meth=%s;@ host=%s;@ headers=[@[%a@]];@ path=%S;@ body=?;@ \
       path_components=%a;@ query=%a@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers self.path
      pp_comp_ self.path_components pp_query self.query

  let pp out self : unit =
    Format.fprintf out
      "{@[meth=%s;@ host=%s;@ headers=[@[%a@]];@ path=%S;@ body=%S;@ \
       path_components=%a;@ query=%a@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers self.path
      self.body pp_comp_ self.path_components pp_query self.query

  (* decode a "chunked" stream into a normal stream *)
  let read_stream_chunked_ ?buf (bs : byte_stream) : byte_stream =
    _debug (fun k -> k "body: start reading chunked stream...");
    Byte_stream.read_chunked ?buf ~fail:(fun s -> Bad_req (400, s)) bs

  let limit_body_size_ ~max_size (bs : byte_stream) : byte_stream =
    _debug (fun k -> k "limit size of body to max-size=%d" max_size);
    Byte_stream.limit_size_to ~max_size ~close_rec:false bs
      ~too_big:(fun size ->
        (* read too much *)
        bad_reqf 413
          "body size was supposed to be %d, but at least %d bytes received"
          max_size size)

  let limit_body_size ~max_size (req : byte_stream t) : byte_stream t =
    { req with body = limit_body_size_ ~max_size req.body }

  (* read exactly [size] bytes from the stream *)
  let read_exactly ~size (bs : byte_stream) : byte_stream =
    _debug (fun k -> k "body: must read exactly %d bytes" size);
    Byte_stream.read_exactly bs ~close_rec:false ~size ~too_short:(fun size ->
        bad_reqf 400 "body is too short by %d bytes" size)

  (* parse request, but not body (yet) *)
  let parse_req_start ?client_addr ~get_time_s ~buf (bs : byte_stream) :
      unit t option resp_result =
    try
      let line = Byte_stream.read_line ~buf bs in
      let start_time = get_time_s () in
      let meth, path, version =
        try
          let meth, path, version =
            Scanf.sscanf line "%s %s HTTP/1.%d\r" (fun x y z -> x, y, z)
          in
          if version != 0 && version != 1 then raise Exit;
          meth, path, version
        with _ ->
          _debug (fun k -> k "invalid request line: `%s`" line);
          raise (Bad_req (400, "Invalid request line"))
      in
      let meth = Meth.of_string meth in
      _debug (fun k -> k "got meth: %s, path %S" (Meth.to_string meth) path);
      let headers = Headers.parse_ ~buf bs in
      let host =
        match Headers.get "Host" headers with
        | None -> bad_reqf 400 "No 'Host' header in request"
        | Some h -> h
      in
      let path_components, query = Tiny_httpd_util.split_query path in
      let path_components = Tiny_httpd_util.split_on_slash path_components in
      let query =
        match Tiny_httpd_util.(parse_query query) with
        | Ok l -> l
        | Error e -> bad_reqf 400 "invalid query: %s" e
      in
      let req =
        {
          meth;
          query;
          host;
          client_addr;
          path;
          path_components;
          headers;
          http_version = 1, version;
          body = ();
          start_time;
        }
      in
      Ok (Some req)
    with
    | End_of_file | Sys_error _ | Unix.Unix_error _ -> Ok None
    | Bad_req (c, s) -> Error (c, s)
    | e -> Error (400, Printexc.to_string e)

  (* parse body, given the headers.
     @param tr_stream a transformation of the input stream. *)
  let parse_body_ ~tr_stream ~buf (req : byte_stream t) :
      byte_stream t resp_result =
    try
      let size =
        match Headers.get_exn "Content-Length" req.headers |> int_of_string with
        | n -> n (* body of fixed size *)
        | exception Not_found -> 0
        | exception _ -> bad_reqf 400 "invalid content-length"
      in
      let body =
        match get_header ~f:String.trim req "Transfer-Encoding" with
        | None -> read_exactly ~size @@ tr_stream req.body
        | Some "chunked" ->
          let bs =
            read_stream_chunked_ ~buf
            @@ tr_stream req.body (* body sent by chunks *)
          in
          if size > 0 then
            limit_body_size_ ~max_size:size bs
          else
            bs
        | Some s -> bad_reqf 500 "cannot handle transfer encoding: %s" s
      in
      Ok { req with body }
    with
    | End_of_file -> Error (400, "unexpected end of file")
    | Bad_req (c, s) -> Error (c, s)
    | e -> Error (400, Printexc.to_string e)

  let read_body_full ?buf ?buf_size (self : byte_stream t) : string t =
    try
      let buf =
        match buf with
        | Some b -> b
        | None -> Buf.create ?size:buf_size ()
      in
      let body = Byte_stream.read_all ~buf self.body in
      { self with body }
    with
    | Bad_req _ as e -> raise e
    | e -> bad_reqf 500 "failed to read body: %s" (Printexc.to_string e)

  module Internal_ = struct
    let parse_req_start ?(buf = Buf.create ()) ~get_time_s bs =
      parse_req_start ~get_time_s ~buf bs |> unwrap_resp_result

    let parse_body ?(buf = Buf.create ()) req bs : _ t =
      parse_body_ ~tr_stream:(fun s -> s) ~buf { req with body = bs }
      |> unwrap_resp_result
  end
end

(*$R
  let q = "GET hello HTTP/1.1\r\nHost: coucou\r\nContent-Length: 11\r\n\r\nsalutationsSOMEJUNK" in
  let str = Tiny_httpd.Byte_stream.of_string q in
  let r = Request.Internal_.parse_req_start ~get_time_s:(fun _ -> 0.) str in
  match r with
  | None -> assert_failure "should parse"
  | Some req ->
    assert_equal (Some "coucou") (Headers.get "Host" req.Request.headers);
    assert_equal (Some "coucou") (Headers.get "host" req.Request.headers);
    assert_equal (Some "11") (Headers.get "content-length" req.Request.headers);
    assert_equal "hello" req.Request.path;
    let req = Request.Internal_.parse_body req str |> Request.read_body_full in
    assert_equal ~printer:(fun s->s) "salutations" req.Request.body;
    ()
*)

module Response = struct
  type body =
    [ `String of string
    | `Stream of byte_stream
    | `Writer of IO.Writer.t
    | `Void ]

  type t = { code: Response_code.t; headers: Headers.t; body: body }

  let set_body body self = { self with body }
  let set_headers headers self = { self with headers }
  let update_headers f self = { self with headers = f self.headers }
  let set_header k v self = { self with headers = Headers.set k v self.headers }
  let set_code code self = { self with code }

  let make_raw ?(headers = []) ~code body : t =
    (* add content length to response *)
    let headers =
      Headers.set "Content-Length" (string_of_int (String.length body)) headers
    in
    { code; headers; body = `String body }

  let make_raw_stream ?(headers = []) ~code body : t =
    let headers = Headers.set "Transfer-Encoding" "chunked" headers in
    { code; headers; body = `Stream body }

  let make_raw_writer ?(headers = []) ~code body : t =
    let headers = Headers.set "Transfer-Encoding" "chunked" headers in
    { code; headers; body = `Writer body }

  let make_void_force_ ?(headers = []) ~code () : t =
    { code; headers; body = `Void }

  let make_void ?(headers = []) ~code () : t =
    let is_ok = code < 200 || code = 204 || code = 304 in
    if is_ok then
      make_void_force_ ~headers ~code ()
    else
      make_raw ~headers ~code "" (* invalid to not have a body *)

  let make_string ?headers r =
    match r with
    | Ok body -> make_raw ?headers ~code:200 body
    | Error (code, msg) -> make_raw ?headers ~code msg

  let make_stream ?headers r =
    match r with
    | Ok body -> make_raw_stream ?headers ~code:200 body
    | Error (code, msg) -> make_raw ?headers ~code msg

  let make_writer ?headers r : t =
    match r with
    | Ok body -> make_raw_writer ?headers ~code:200 body
    | Error (code, msg) -> make_raw ?headers ~code msg

  let make ?headers r : t =
    match r with
    | Ok (`String body) -> make_raw ?headers ~code:200 body
    | Ok (`Stream body) -> make_raw_stream ?headers ~code:200 body
    | Ok `Void -> make_void ?headers ~code:200 ()
    | Ok (`Writer f) -> make_raw_writer ?headers ~code:200 f
    | Error (code, msg) -> make_raw ?headers ~code msg

  let fail ?headers ~code fmt =
    Printf.ksprintf (fun msg -> make_raw ?headers ~code msg) fmt

  let fail_raise ~code fmt =
    Printf.ksprintf (fun msg -> raise (Bad_req (code, msg))) fmt

  let pp out self : unit =
    let pp_body out = function
      | `String s -> Format.fprintf out "%S" s
      | `Stream _ -> Format.pp_print_string out "<stream>"
      | `Writer _ -> Format.pp_print_string out "<writer>"
      | `Void -> ()
    in
    Format.fprintf out "{@[code=%d;@ headers=[@[%a@]];@ body=%a@]}" self.code
      Headers.pp self.headers pp_body self.body

  let output_ ~buf (oc : IO.Out_channel.t) (self : t) : unit =
    (* double indirection:
       - print into [buffer] using [bprintf]
       - transfer to [buf_] so we can output from there *)
    let tmp_buffer = Buffer.create 32 in
    Buf.clear buf;

    (* write start of reply *)
    Printf.bprintf tmp_buffer "HTTP/1.1 %d %s\r\n" self.code
      (Response_code.descr self.code);
    Buf.add_buffer buf tmp_buffer;
    Buffer.clear tmp_buffer;

    let body, is_chunked =
      match self.body with
      | `String s when String.length s > 1024 * 500 ->
        (* chunk-encode large bodies *)
        `Writer (IO.Writer.of_string s), true
      | `String _ as b -> b, false
      | `Stream _ as b -> b, true
      | `Writer _ as b -> b, true
      | `Void as b -> b, false
    in
    let headers =
      if is_chunked then
        self.headers
        |> Headers.set "transfer-encoding" "chunked"
        |> Headers.remove "content-length"
      else
        self.headers
    in
    let self = { self with headers; body } in
    _debug (fun k ->
        k "output response: %s"
          (Format.asprintf "%a" pp { self with body = `String "<...>" }));

    (* write headers, using [buf] to batch writes *)
    List.iter
      (fun (k, v) ->
        Printf.bprintf tmp_buffer "%s: %s\r\n" k v;
        Buf.add_buffer buf tmp_buffer;
        Buffer.clear tmp_buffer)
      headers;

    IO.Out_channel.output_buf oc buf;
    IO.Out_channel.output_string oc "\r\n";
    Buf.clear buf;

    (match body with
    | `String "" | `Void -> ()
    | `String s -> IO.Out_channel.output_string oc s
    | `Writer w ->
      (* use buffer to chunk encode [w] *)
      let oc' = IO.Out_channel.chunk_encoding ~buf ~close_rec:false oc in
      (try
         IO.Writer.write oc' w;
         IO.Out_channel.close oc'
       with e ->
         IO.Out_channel.close oc';
         raise e)
    | `Stream str ->
      (try
         Byte_stream.output_chunked' ~buf oc str;
         Byte_stream.close str
       with e ->
         Byte_stream.close str;
         raise e));
    IO.Out_channel.flush oc
end

(* semaphore, for limiting concurrency. *)
module Sem_ = struct
  type t = { mutable n: int; max: int; mutex: Mutex.t; cond: Condition.t }

  let create n =
    if n <= 0 then invalid_arg "Semaphore.create";
    { n; max = n; mutex = Mutex.create (); cond = Condition.create () }

  let acquire m t =
    Mutex.lock t.mutex;
    while t.n < m do
      Condition.wait t.cond t.mutex
    done;
    assert (t.n >= m);
    t.n <- t.n - m;
    Condition.broadcast t.cond;
    Mutex.unlock t.mutex

  let release m t =
    Mutex.lock t.mutex;
    t.n <- t.n + m;
    Condition.broadcast t.cond;
    Mutex.unlock t.mutex

  let num_acquired t = t.max - t.n
end

module Route = struct
  type path = string list (* split on '/' *)

  type (_, _) comp =
    | Exact : string -> ('a, 'a) comp
    | Int : (int -> 'a, 'a) comp
    | String : (string -> 'a, 'a) comp
    | String_urlencoded : (string -> 'a, 'a) comp

  type (_, _) t =
    | Fire : ('b, 'b) t
    | Rest : { url_encoded: bool } -> (string -> 'b, 'b) t
    | Compose : ('a, 'b) comp * ('b, 'c) t -> ('a, 'c) t

  let return = Fire
  let rest_of_path = Rest { url_encoded = false }
  let rest_of_path_urlencoded = Rest { url_encoded = true }
  let ( @/ ) a b = Compose (a, b)
  let string = String
  let string_urlencoded = String_urlencoded
  let int = Int
  let exact (s : string) = Exact s

  let exact_path (s : string) tail =
    let rec fn = function
      | [] -> tail
      | "" :: ls -> fn ls
      | s :: ls -> exact s @/ fn ls
    in
    fn (String.split_on_char '/' s)

  let rec eval : type a b. path -> (a, b) t -> a -> b option =
   fun path route f ->
    match path, route with
    | [], Fire -> Some f
    | _, Fire -> None
    | _, Rest { url_encoded } ->
      let whole_path = String.concat "/" path in
      (match
         if url_encoded then (
           match Tiny_httpd_util.percent_decode whole_path with
           | Some s -> s
           | None -> raise_notrace Exit
         ) else
           whole_path
       with
      | whole_path -> Some (f whole_path)
      | exception Exit -> None)
    | c1 :: path', Compose (comp, route') ->
      (match comp with
      | Int ->
        (match int_of_string c1 with
        | i -> eval path' route' (f i)
        | exception _ -> None)
      | String -> eval path' route' (f c1)
      | String_urlencoded ->
        (match Tiny_httpd_util.percent_decode c1 with
        | None -> None
        | Some s -> eval path' route' (f s))
      | Exact s ->
        if s = c1 then
          eval path' route' f
        else
          None)
    | [], Compose (String, Fire) -> Some (f "") (* trailing *)
    | [], Compose (String_urlencoded, Fire) -> Some (f "") (* trailing *)
    | [], Compose _ -> None

  let bpf = Printf.bprintf

  let rec pp_ : type a b. Buffer.t -> (a, b) t -> unit =
   fun out -> function
    | Fire -> bpf out "/"
    | Rest { url_encoded } ->
      bpf out "<rest_of_url%s>"
        (if url_encoded then
          "_urlencoded"
        else
          "")
    | Compose (Exact s, tl) -> bpf out "%s/%a" s pp_ tl
    | Compose (Int, tl) -> bpf out "<int>/%a" pp_ tl
    | Compose (String, tl) -> bpf out "<str>/%a" pp_ tl
    | Compose (String_urlencoded, tl) -> bpf out "<enc_str>/%a" pp_ tl

  let to_string x =
    let b = Buffer.create 16 in
    pp_ b x;
    Buffer.contents b

  let pp out x = Format.pp_print_string out (to_string x)
end

module Middleware = struct
  type handler = byte_stream Request.t -> resp:(Response.t -> unit) -> unit
  type t = handler -> handler

  (** Apply a list of middlewares to [h] *)
  let apply_l (l : t list) (h : handler) : handler =
    List.fold_right (fun m h -> m h) l h

  let[@inline] nil : t = fun h -> h
end

(* a request handler. handles a single request. *)
type cb_path_handler = IO.Out_channel.t -> Middleware.handler

module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit

  val send_event :
    ?event:string -> ?id:string -> ?retry:string -> data:string -> unit -> unit

  val close : unit -> unit
end

type server_sent_generator = (module SERVER_SENT_GENERATOR)

module type IO_BACKEND = sig
  val init_addr : unit -> string
  val init_port : unit -> int

  val get_time_s : unit -> float
  (** obtain the current timestamp in seconds. *)

  val tcp_server : unit -> Tiny_httpd_io.TCP_server.builder
  (** Server  that can listen on a port and handle clients. *)
end

type t = {
  backend: (module IO_BACKEND);
  mutable tcp_server: IO.TCP_server.t option;
  buf_size: int;
  mutable handler: string Request.t -> Response.t;
      (** toplevel handler, if any *)
  mutable middlewares: (int * Middleware.t) list;  (** Global middlewares *)
  mutable middlewares_sorted: (int * Middleware.t) list lazy_t;
      (** sorted version of {!middlewares} *)
  mutable path_handlers:
    (unit Request.t -> cb_path_handler resp_result option) list;
      (** path handlers *)
  buf_pool: Buf.t Pool.t;
}

let get_addr_ sock =
  match Unix.getsockname sock with
  | Unix.ADDR_INET (addr, port) -> addr, port
  | _ -> invalid_arg "httpd: address is not INET"

let addr (self : t) =
  match self.tcp_server with
  | None ->
    let (module B) = self.backend in
    B.init_addr ()
  | Some s -> fst @@ s.endpoint ()

let port (self : t) =
  match self.tcp_server with
  | None ->
    let (module B) = self.backend in
    B.init_port ()
  | Some s -> snd @@ s.endpoint ()

let active_connections (self : t) =
  match self.tcp_server with
  | None -> 0
  | Some s -> s.active_connections ()

let add_middleware ~stage self m =
  let stage =
    match stage with
    | `Encoding -> 0
    | `Stage n when n < 1 -> invalid_arg "add_middleware: bad stage"
    | `Stage n -> n
  in
  self.middlewares <- (stage, m) :: self.middlewares;
  self.middlewares_sorted <-
    lazy
      (List.stable_sort (fun (s1, _) (s2, _) -> compare s1 s2) self.middlewares)

let add_decode_request_cb self f =
  (* turn it into a middleware *)
  let m h req ~resp =
    (* see if [f] modifies the stream *)
    let req0 = { req with Request.body = () } in
    match f req0 with
    | None -> h req ~resp (* pass through *)
    | Some (req1, tr_stream) ->
      let req = { req1 with Request.body = tr_stream req.Request.body } in
      h req ~resp
  in
  add_middleware self ~stage:`Encoding m

let add_encode_response_cb self f =
  let m h req ~resp =
    h req ~resp:(fun r ->
        let req0 = { req with Request.body = () } in
        (* now transform [r] if we want to *)
        match f req0 r with
        | None -> resp r
        | Some r' -> resp r')
  in
  add_middleware self ~stage:`Encoding m

let set_top_handler self f = self.handler <- f

(* route the given handler.
   @param tr_req wraps the actual concrete function returned by the route
   and makes it into a handler. *)
let add_route_handler_ ?(accept = fun _req -> Ok ()) ?(middlewares = []) ?meth
    ~tr_req self (route : _ Route.t) f =
  let ph req : cb_path_handler resp_result option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      (match Route.eval req.Request.path_components route f with
      | Some handler ->
        (* we have a handler, do we accept the request based on its headers? *)
        (match accept req with
        | Ok () ->
          Some
            (Ok
               (fun oc ->
                 Middleware.apply_l middlewares @@ fun req ~resp ->
                 tr_req oc req ~resp handler))
        | Error _ as e -> Some e)
      | None -> None (* path didn't match *))
  in
  self.path_handlers <- ph :: self.path_handlers

let add_route_handler (type a) ?accept ?middlewares ?meth self
    (route : (a, _) Route.t) (f : _) : unit =
  let tr_req _oc req ~resp f =
    let req =
      Pool.with_resource self.buf_pool @@ fun buf ->
      Request.read_body_full ~buf req
    in
    resp (f req)
  in
  add_route_handler_ ?accept ?middlewares ?meth self route ~tr_req f

let add_route_handler_stream ?accept ?middlewares ?meth self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  add_route_handler_ ?accept ?middlewares ?meth self route ~tr_req f

let[@inline] _opt_iter ~f o =
  match o with
  | None -> ()
  | Some x -> f x

exception Exit_SSE

let add_route_server_sent_handler ?accept self route f =
  let tr_req (oc : IO.Out_channel.t) req ~resp f =
    let req =
      Pool.with_resource self.buf_pool @@ fun buf ->
      Request.read_body_full ~buf req
    in
    let headers =
      ref Headers.(empty |> set "content-type" "text/event-stream")
    in

    (* send response once *)
    let resp_sent = ref false in
    let send_response_idempotent_ () =
      if not !resp_sent then (
        resp_sent := true;
        (* send 200 response now *)
        let initial_resp =
          Response.make_void_force_ ~headers:!headers ~code:200 ()
        in
        resp initial_resp
      )
    in

    let[@inline] writef fmt =
      Printf.ksprintf (IO.Out_channel.output_string oc) fmt
    in

    let send_event ?event ?id ?retry ~data () : unit =
      send_response_idempotent_ ();
      _opt_iter event ~f:(fun e -> writef "event: %s\n" e);
      _opt_iter id ~f:(fun e -> writef "id: %s\n" e);
      _opt_iter retry ~f:(fun e -> writef "retry: %s\n" e);
      let l = String.split_on_char '\n' data in
      List.iter (fun s -> writef "data: %s\n" s) l;
      IO.Out_channel.output_string oc "\n";
      (* finish group *)
      IO.Out_channel.flush oc
    in
    let module SSG = struct
      let set_headers h =
        if not !resp_sent then (
          headers := List.rev_append h !headers;
          send_response_idempotent_ ()
        )

      let send_event = send_event
      let close () = raise Exit_SSE
    end in
    try f req (module SSG : SERVER_SENT_GENERATOR)
    with Exit_SSE -> IO.Out_channel.close oc
  in
  add_route_handler_ self ?accept ~meth:`GET route ~tr_req f

let get_max_connection_ ?(max_connections = 64) () : int =
  let max_connections = max 4 max_connections in
  max_connections

let create_from ?(buf_size = 16 * 1_024) ?(middlewares = []) ~backend () : t =
  let handler _req = Response.fail ~code:404 "no top handler" in
  let self =
    {
      backend;
      tcp_server = None;
      handler;
      buf_size;
      path_handlers = [];
      middlewares = [];
      middlewares_sorted = lazy [];
      buf_pool =
        Pool.create ~clear:Buf.clear
          ~mk_item:(fun () -> Buf.create ~size:buf_size ())
          ();
    }
  in
  List.iter (fun (stage, m) -> add_middleware self ~stage m) middlewares;
  self

let is_ipv6_str addr : bool = String.contains addr ':'

module Unix_tcp_server_ = struct
  type t = {
    addr: string;
    port: int;
    max_connections: int;
    sem_max_connections: Sem_.t;
        (** semaphore to restrict the number of active concurrent connections *)
    mutable sock: Unix.file_descr option;  (** Socket *)
    new_thread: (unit -> unit) -> unit;
    timeout: float;
    masksigpipe: bool;
    mutable running: bool; (* TODO: use an atomic? *)
  }

  let to_tcp_server (self : t) : IO.TCP_server.builder =
    {
      IO.TCP_server.serve =
        (fun ~after_init ~handle () : unit ->
          if self.masksigpipe then
            ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ] : _ list);
          let sock, should_bind =
            match self.sock with
            | Some s ->
              ( s,
                false
                (* Because we're getting a socket from the caller (e.g. systemd) *)
              )
            | None ->
              ( Unix.socket
                  (if is_ipv6_str self.addr then
                    Unix.PF_INET6
                  else
                    Unix.PF_INET)
                  Unix.SOCK_STREAM 0,
                true (* Because we're creating the socket ourselves *) )
          in
          Unix.clear_nonblock sock;
          Unix.setsockopt_optint sock Unix.SO_LINGER None;
          if should_bind then (
            let inet_addr = Unix.inet_addr_of_string self.addr in
            Unix.setsockopt sock Unix.SO_REUSEADDR true;
            Unix.bind sock (Unix.ADDR_INET (inet_addr, self.port));
            let n_listen = 2 * self.max_connections in
            Unix.listen sock n_listen
          );

          self.sock <- Some sock;

          let tcp_server =
            {
              IO.TCP_server.stop = (fun () -> self.running <- false);
              running = (fun () -> self.running);
              active_connections =
                (fun () -> Sem_.num_acquired self.sem_max_connections - 1);
              endpoint =
                (fun () ->
                  let addr, port = get_addr_ sock in
                  Unix.string_of_inet_addr addr, port);
            }
          in
          after_init tcp_server;

          (* how to handle a single client *)
          let handle_client_unix_ (client_sock : Unix.file_descr) (client_addr : Unix.sockaddr) : unit =
            Unix.(setsockopt_float client_sock SO_RCVTIMEO self.timeout);
            Unix.(setsockopt_float client_sock SO_SNDTIMEO self.timeout);
            let oc =
              IO.Out_channel.of_out_channel
              @@ Unix.out_channel_of_descr client_sock
            in
            let ic = IO.In_channel.of_unix_fd client_sock in
            handle.handle ~client_addr ic oc;
            _debug (fun k -> k "done with client, exiting");
            (try Unix.close client_sock
             with e ->
               _debug (fun k ->
                   k "error when closing sock: %s" (Printexc.to_string e)));
            ()
          in

          while self.running do
            (* limit concurrency *)
            Sem_.acquire 1 self.sem_max_connections;
            try
              let client_sock, client_addr = Unix.accept sock in
              Unix.setsockopt client_sock Unix.TCP_NODELAY true;
              self.new_thread (fun () ->
                  try
                    handle_client_unix_ client_sock client_addr;
                    Sem_.release 1 self.sem_max_connections
                  with e ->
                    (try Unix.close client_sock with _ -> ());
                    Sem_.release 1 self.sem_max_connections;
                    raise e)
            with e ->
              Sem_.release 1 self.sem_max_connections;
              _debug (fun k ->
                  k "Unix.accept or Thread.create raised an exception: %s"
                    (Printexc.to_string e))
          done;
          ());
    }
end

let create ?(masksigpipe = true) ?max_connections ?(timeout = 0.0) ?buf_size
    ?(get_time_s = Unix.gettimeofday)
    ?(new_thread = fun f -> ignore (Thread.create f () : Thread.t))
    ?(addr = "127.0.0.1") ?(port = 8080) ?sock ?middlewares () : t =
  let max_connections = get_max_connection_ ?max_connections () in
  let server =
    {
      Unix_tcp_server_.addr;
      new_thread;
      running = true;
      port;
      sock;
      max_connections;
      sem_max_connections = Sem_.create max_connections;
      masksigpipe;
      timeout;
    }
  in
  let tcp_server_builder = Unix_tcp_server_.to_tcp_server server in
  let module B = struct
    let init_addr () = addr
    let init_port () = port
    let get_time_s = get_time_s
    let tcp_server () = tcp_server_builder
  end in
  let backend = (module B : IO_BACKEND) in
  create_from ?buf_size ?middlewares ~backend ()

let stop (self : t) =
  match self.tcp_server with
  | None -> ()
  | Some s -> s.stop ()

let running (self : t) =
  match self.tcp_server with
  | None -> false
  | Some s -> s.running ()

let find_map f l =
  let rec aux f = function
    | [] -> None
    | x :: l' ->
      (match f x with
      | Some _ as res -> res
      | None -> aux f l')
  in
  aux f l

(* handle client on [ic] and [oc] *)
let client_handle_for (self : t) ?client_addr ic oc : unit =
  Pool.with_resource self.buf_pool @@ fun buf ->
  Pool.with_resource self.buf_pool @@ fun buf_res ->
  let is = Byte_stream.of_input ~buf_size:self.buf_size ic in
  let continue = ref true in
  while !continue && running self do
    _debug (fun k -> k "read next request");
    let (module B) = self.backend in
    match Request.parse_req_start ?client_addr ~get_time_s:B.get_time_s ~buf is with
    | Ok None -> continue := false (* client is done *)
    | Error (c, s) ->
      (* connection error, close *)
      let res = Response.make_raw ~code:c s in
      (try Response.output_ ~buf:buf_res oc res with Sys_error _ -> ());
      continue := false
    | Ok (Some req) ->
      _debug (fun k -> k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

      if Request.close_after_req req then continue := false;

      (try
         (* is there a handler for this path? *)
         let base_handler =
           match find_map (fun ph -> ph req) self.path_handlers with
           | Some f -> unwrap_resp_result f
           | None ->
             fun _oc req ~resp ->
               let body_str =
                 Pool.with_resource self.buf_pool @@ fun buf ->
                 Request.read_body_full ~buf req
               in
               resp (self.handler body_str)
         in

         (* handle expect/continue *)
         (match Request.get_header ~f:String.trim req "Expect" with
         | Some "100-continue" ->
           _debug (fun k -> k "send back: 100 CONTINUE");
           Response.output_ ~buf:buf_res oc (Response.make_raw ~code:100 "")
         | Some s -> bad_reqf 417 "unknown expectation %s" s
         | None -> ());

         (* apply middlewares *)
         let handler oc =
           List.fold_right
             (fun (_, m) h -> m h)
             (Lazy.force self.middlewares_sorted)
             (base_handler oc)
         in

         (* now actually read request's body into a stream *)
         let req =
           Request.parse_body_
             ~tr_stream:(fun s -> s)
             ~buf { req with body = is }
           |> unwrap_resp_result
         in

         (* how to reply *)
         let resp r =
           try
             if Headers.get "connection" r.Response.headers = Some "close" then
               continue := false;
             Response.output_ ~buf:buf_res oc r
           with Sys_error _ -> continue := false
         in

         (* call handler *)
         try handler oc req ~resp with Sys_error _ -> continue := false
       with
      | Sys_error _ -> continue := false
      (* connection broken somehow *)
      | Bad_req (code, s) ->
        continue := false;
        Response.output_ ~buf:buf_res oc @@ Response.make_raw ~code s
      | e ->
        continue := false;
        Response.output_ ~buf:buf_res oc
        @@ Response.fail ~code:500 "server error: %s" (Printexc.to_string e))
  done

let client_handler (self : t) : IO.TCP_server.conn_handler =
  { IO.TCP_server.handle = client_handle_for self }

let is_ipv6 (self : t) =
  let (module B) = self.backend in
  is_ipv6_str (B.init_addr ())

let run_exn ?(after_init = ignore) (self : t) : unit =
  let (module B) = self.backend in
  let server = B.tcp_server () in
  server.serve
    ~after_init:(fun tcp_server ->
      self.tcp_server <- Some tcp_server;
      after_init ())
    ~handle:(client_handler self) ()

let run ?after_init self : _ result =
  try Ok (run_exn ?after_init self) with e -> Error e
