type byte_stream = {
  bs_fill_buf: unit -> (bytes * int * int);
  bs_consume: int -> unit;
  bs_close: unit -> unit;
}
(** A buffer input stream, with a view into the current buffer (or refill if empty),
    and a function to consume [n] bytes *)

let _debug_on = ref (
  match String.trim @@ Sys.getenv "HTTP_DBG" with
  | "" -> false | _ -> true | exception _ -> false
)
let _enable_debug b = _debug_on := b
let _debug k =
  if !_debug_on then (
    k (fun fmt->
       Printf.fprintf stdout "[http.thread %d]: " Thread.(id @@ self());
       Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") stdout fmt)
  )

module Buf_ = struct
  type t = {
    mutable bytes: bytes;
    mutable i: int;
  }

  let create ?(size=4_096) () : t =
    { bytes=Bytes.make size ' '; i=0 }

  let size self = self.i
  let bytes_slice self = self.bytes
  let clear self : unit =
    if Bytes.length self.bytes > 4_096 * 1_024 then (
      self.bytes <- Bytes.make 4096 ' '; (* free big buffer *)
    );
    self.i <- 0

  let resize self new_size : unit =
    let new_buf = Bytes.make new_size ' ' in
    Bytes.blit self.bytes 0 new_buf 0 self.i;
    self.bytes <- new_buf

  let add_bytes (self:t) s i len : unit =
    if self.i + len >= Bytes.length self.bytes then (
      resize self (self.i + self.i / 2 + len + 10);
    );
    Bytes.blit s i self.bytes self.i len;
    self.i <- self.i + len

  let contents (self:t) : string = Bytes.sub_string self.bytes 0 self.i

  let contents_and_clear (self:t) : string =
    let x = contents self in
    clear self;
    x
end

module Byte_stream = struct
  type t = byte_stream

  let close self = self.bs_close()

  let empty = {
    bs_fill_buf=(fun () -> Bytes.empty, 0, 0);
    bs_consume=(fun _ -> ());
    bs_close=(fun () -> ());
  }

  let of_chan_ ~close ic : t =
    let i = ref 0 in
    let len = ref 0 in
    let buf = Bytes.make 4096 ' ' in
    { bs_fill_buf=(fun () ->
      if !i >= !len then (
        i := 0;
        len := input ic buf 0 (Bytes.length buf);
      );
      buf, !i,!len - !i);
      bs_consume=(fun n -> i := !i + n);
      bs_close=(fun () -> close ic)
    }

  let of_chan = of_chan_ ~close:close_in
  let of_chan_close_noerr = of_chan_ ~close:close_in_noerr

  let rec iter f (self:t) : unit =
    let s, i, len = self.bs_fill_buf () in
    if len=0 then (
      self.bs_close();
    ) else (
      f s i len;
      self.bs_consume len;
      (iter [@tailcall]) f self
    )

  let to_chan (oc:out_channel) (self:t) =
    iter (fun s i len -> output oc s i len) self

  let of_bytes ?(i=0) ?len s : t =
    (* invariant: !i+!len is constant *)
    let len =
      ref (
        match len with
        | Some n ->
          if n > Bytes.length s - i then invalid_arg "Byte_stream.of_bytes";
          n
        | None -> Bytes.length s - i
      )
    in
    let i = ref i in
    { bs_fill_buf=(fun () -> s, !i, !len);
      bs_close=(fun () -> len := 0);
      bs_consume=(fun n -> assert (n>=0 && n<= !len); i := !i + n; len := !len - n);
    }

  let of_string s : t =
    of_bytes (Bytes.unsafe_of_string s)

  let with_file file f =
    let ic = open_in file in
    try
      let x = f (of_chan ic) in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  let read_all ?(buf=Buf_.create()) (self:t) : string =
    let continue = ref true in
    while !continue do
      let s, i, len = self.bs_fill_buf () in
      _debug (fun k->k "read-all: got i=%d, len=%d, bufsize %d" i len (Buf_.size buf));
      if len > 0 then (
        Buf_.add_bytes buf s i len;
        self.bs_consume len;
      );
      assert (len >= 0);
      if len = 0 then (
        continue := false
      )
    done;
    Buf_.contents_and_clear buf

  (* put [n] bytes from the input into bytes *)
  let read_exactly_ ~too_short (self:t) (bytes:bytes) (n:int) : unit =
    assert (Bytes.length bytes >= n);
    let offset = ref 0 in
    while !offset < n do
      let s, i, len = self.bs_fill_buf () in
      let n_read = min len (n- !offset) in
      Bytes.blit s i bytes !offset n_read;
      offset := !offset + n_read;
      self.bs_consume n_read;
      if n_read=0 then too_short();
    done

  (* read a line into the buffer, after clearing it. *)
  let read_line_into (self:t) ~buf : unit =
    Buf_.clear buf;
    let continue = ref true in
    while !continue do
      let s, i, len = self.bs_fill_buf () in
      if len=0 then (
        continue := false;
        if Buf_.size buf = 0 then raise End_of_file;
      );
      let j = ref i in
      while !j < i+len && Bytes.get s !j <> '\n' do
        incr j
      done;
      if !j-i < len then (
        assert (Bytes.get s !j = '\n');
        Buf_.add_bytes buf s i (!j-i); (* without \n *)
        self.bs_consume (!j-i+1); (* remove \n *)
        continue := false
      ) else (
        Buf_.add_bytes buf s i len;
        self.bs_consume len;
      )
    done

  (* new stream with maximum size [max_size].
     @param close_rec if true, closing this will also close the input stream
     @param too_big called with read size if the max size is reached *)
  let limit_size_to ~close_rec ~max_size ~too_big (self:t) : t =
    let size = ref 0 in
    let continue = ref true in
    { bs_fill_buf =
        (fun () ->
          if !continue then self.bs_fill_buf() else Bytes.empty, 0, 0);
      bs_close=(fun () ->
          if close_rec then self.bs_close ());
      bs_consume = (fun n ->
          size := !size + n;
          if !size > max_size then (
            continue := false;
            too_big !size
          ) else (
            self.bs_consume n
          ));
    }

  (* read exactly [size] bytes from the stream *)
  let read_exactly ~close_rec ~size ~too_short (self:t) : t =
    if size=0 then (
      empty
    ) else (
      let size = ref size in
      { bs_fill_buf = (fun () ->
            (* must not block on [self] if we're done *)
            if !size = 0 then Bytes.empty, 0, 0
            else (
              let buf, i, len = self.bs_fill_buf () in
              let len = min len !size in
              if len = 0 && !size > 0 then (
                too_short !size;
              );
              buf, i, len
            )
          );
        bs_close=(fun () ->
            (* close underlying stream if [close_rec] *)
            if close_rec then self.bs_close();
            size := 0);
        bs_consume = (fun n ->
            let n = min n !size in
            size := !size - n;
            self.bs_consume n);
      }
    )

  let read_line ?(buf=Buf_.create()) self : string =
    read_line_into self ~buf;
    Buf_.contents buf
end

exception Bad_req of int * string
let bad_reqf c fmt = Printf.ksprintf (fun s ->raise (Bad_req (c,s))) fmt

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
  | Error (c,s) -> raise (Bad_req (c,s))

module Meth = struct
  type t = [
    | `GET
    | `PUT
    | `POST
    | `HEAD
    | `DELETE
  ]

  let to_string = function
    | `GET -> "GET"
    | `PUT -> "PUT"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `DELETE -> "DELETE"
  let pp out s = Format.pp_print_string out (to_string s)

  let of_string = function
    | "GET" -> `GET
    | "PUT" -> `PUT
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | "DELETE" -> `DELETE
    | s -> bad_reqf 400 "unknown method %S" s
end

module Headers = struct
  type t = (string * string) list
  let empty = []
  let contains name headers =
    let name' = String.lowercase_ascii name in
    List.exists (fun (n, _) -> name'=n) headers
  let get_exn ?(f=fun x->x) x h =
    let x' = String.lowercase_ascii x in
    List.assoc x' h |> f
  let get ?(f=fun x -> x) x h =
    try Some (get_exn ~f x h) with Not_found -> None
  let remove x h =
    let x' = String.lowercase_ascii x in
    List.filter (fun (k,_) -> k<>x') h
  let set x y h =
    let x' = String.lowercase_ascii x in
    (x',y) :: List.filter (fun (k,_) -> k<>x') h
  let pp out l =
    let pp_pair out (k,v) = Format.fprintf out "@[<h>%s: %s@]" k v in
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l

  (*  token = 1*tchar
  tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
           / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
  Reference: https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 *)
  let is_tchar = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^'
    | '_' | '`'  | '|' | '~' -> true
    | _ -> false

  let for_all pred s =
    try String.iter (fun c->if not (pred c) then raise Exit) s; true
    with Exit -> false

  let parse_ ~buf (bs:byte_stream) : t =
    let rec loop acc =
      let line = Byte_stream.read_line ~buf bs in
      _debug (fun k->k  "parsed header line %S" line);
      if line = "\r" then (
        acc
      ) else (
        let k,v =
          try
            let i = String.index line ':' in
            let k = String.sub line 0 i in
            if not (for_all is_tchar k) then (
              invalid_arg (Printf.sprintf "Invalid header key: %S" k));
            let v = String.sub line (i+1) (String.length line-i-1) |> String.trim in
            k,v
          with _ -> bad_reqf 400 "invalid header line: %S" line
        in
        loop ((String.lowercase_ascii k,v)::acc)
      )
    in
    loop []
end

module Request = struct
  type 'body t = {
    meth: Meth.t;
    host: string;
    headers: Headers.t;
    path: string;
    path_components: string list;
    query: (string*string) list;
    body: 'body;
  }

  let headers self = self.headers
  let host self = self.host
  let meth self = self.meth
  let path self = self.path
  let body self = self.body

  let non_query_path self = Tiny_httpd_util.get_non_query_path self.path

  let query self = self.query
  let get_header ?f self h = Headers.get ?f h self.headers
  let get_header_int self h = match get_header self h with
    | Some x -> (try Some (int_of_string x) with _ -> None)
    | None -> None
  let set_header self k v = {self with headers=Headers.set k v self.headers}

  let pp_comp_ out comp =
    Format.fprintf out "[%s]"
      (String.concat ";" @@ List.map (Printf.sprintf "%S") comp)
  let pp_query out q =
    Format.fprintf out "[%s]"
      (String.concat ";" @@
       List.map (fun (a,b) -> Printf.sprintf "%S,%S" a b) q)
  let pp_ out self : unit =
    Format.fprintf out "{@[meth=%s;@ host=%s;@ headers=[@[%a@]];@ \
                        path=%S;@ body=?;@ path_components=%a;@ query=%a@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers self.path
      pp_comp_ self.path_components pp_query self.query
  let pp out self : unit =
    Format.fprintf out "{@[meth=%s;@ host=%s;@ headers=[@[%a@]];@ path=%S;@ \
                        body=%S;@ path_components=%a;@ query=%a@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers
      self.path self.body pp_comp_ self.path_components pp_query self.query

  (* decode a "chunked" stream into a normal stream *)
  let read_stream_chunked_ ?(buf=Buf_.create()) (bs:byte_stream) : byte_stream =
    _debug (fun k->k "body: start reading chunked stream...");
    let first = ref true in
    let read_next_chunk_len () : int =
      if !first then (
        first := false
      ) else (
        let line = Byte_stream.read_line ~buf bs in
        if String.trim line <> "" then bad_reqf 400 "expected crlf between chunks";
      );
      let line = Byte_stream.read_line ~buf bs in
      (* parse chunk length, ignore extensions *)
      let chunk_size = (
        if String.trim line = "" then 0
        else
          try Scanf.sscanf line "%x %s@\r" (fun n _ext -> n)
          with _ -> bad_reqf 400 "cannot read chunk size from line %S" line
      ) in
      chunk_size
    in
    let refill = ref true in
    let bytes = Bytes.make (16 * 4096)  ' ' in (* internal buffer, 16kb *)
    let offset = ref 0 in
    let len = ref 0 in
    let chunk_size = ref 0 in
    { bs_fill_buf=
        (fun () ->
           (* do we need to refill? *)
           if !offset >= !len then (
             if !chunk_size = 0 && !refill then (
               chunk_size := read_next_chunk_len();
               (* _debug (fun k->k"read next chunk of size %d" !chunk_size); *)
             );
             offset := 0;
             len := 0;
             if !chunk_size > 0 then (
               (* read the whole chunk, or [Bytes.length bytes] of it *)
               let to_read = min !chunk_size (Bytes.length bytes) in
               Byte_stream.read_exactly_
                 ~too_short:(fun () -> bad_reqf 400 "chunk is too short")
                 bs bytes to_read;
               len := to_read;
               chunk_size := !chunk_size - to_read;
             ) else (
               refill := false; (* stream is finished *)
             )
           );
           bytes, !offset, !len
        );
      bs_consume=(fun n -> offset := !offset + n);
      bs_close=(fun () ->
          (* close this overlay, do not close underlying stream *)
          len := 0; refill:= false);
    }

  let limit_body_size_ ~max_size (bs:byte_stream) : byte_stream =
    _debug (fun k->k "limit size of body to max-size=%d" max_size);
    Byte_stream.limit_size_to ~max_size ~close_rec:false bs
      ~too_big:(fun size ->
          (* read too much *)
          bad_reqf 413
            "body size was supposed to be %d, but at least %d bytes received"
            max_size size
        )

  let limit_body_size ~max_size (req:byte_stream t) : byte_stream t =
    { req with body=limit_body_size_ ~max_size req.body }

  (* read exactly [size] bytes from the stream *)
  let read_exactly ~size (bs:byte_stream) : byte_stream =
    _debug (fun k->k "body: must read exactly %d bytes" size);
    Byte_stream.read_exactly bs ~close_rec:false
      ~size ~too_short:(fun size ->
          bad_reqf 400 "body is too short by %d bytes" size
        )

  (* parse request, but not body (yet) *)
  let parse_req_start ~buf (bs:byte_stream) : unit t option resp_result =
    try
      let line = Byte_stream.read_line ~buf bs in
      let meth, path =
        try
          let m, p, v = Scanf.sscanf line "%s %s HTTP/1.%d\r" (fun x y z->x,y,z) in
          if v != 0 && v != 1 then raise Exit;
          m, p
        with _ ->
          _debug (fun k->k "invalid request line: `%s`" line);
          raise (Bad_req (400, "Invalid request line"))
      in
      let meth = Meth.of_string meth in
      _debug (fun k->k "got meth: %s, path %S" (Meth.to_string meth) path);
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
      Ok (Some {meth; query; host; path; path_components;
                headers; body=()})
    with
    | End_of_file | Sys_error _ -> Ok None
    | Bad_req (c,s) -> Error (c,s)
    | e ->
      Error (400, Printexc.to_string e)

  (* parse body, given the headers.
     @param tr_stream a transformation of the input stream. *)
  let parse_body_ ~tr_stream ~buf (req:byte_stream t) : byte_stream t resp_result =
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
            read_stream_chunked_ ~buf @@ tr_stream req.body (* body sent by chunks *)
          in
          if size>0 then limit_body_size_ ~max_size:size bs else bs
        | Some s -> bad_reqf 500 "cannot handle transfer encoding: %s" s
      in
      Ok {req with body}
    with
    | End_of_file -> Error (400, "unexpected end of file")
    | Bad_req (c,s) -> Error (c,s)
    | e ->
      Error (400, Printexc.to_string e)

  let read_body_full (self:byte_stream t) : string t =
    try
      let body = Byte_stream.read_all self.body in
      { self with body }
    with
    | Bad_req _ as e -> raise e
    | e -> bad_reqf 500 "failed to read body: %s" (Printexc.to_string e)

  module Internal_ = struct
    let parse_req_start ?(buf=Buf_.create()) bs =
      parse_req_start ~buf bs |> unwrap_resp_result

    let parse_body ?(buf=Buf_.create()) req bs : _ t =
      parse_body_ ~tr_stream:(fun s->s) ~buf {req with body=bs} |> unwrap_resp_result
  end
end

(*$R
  let q = "GET hello HTTP/1.1\r\nHost: coucou\r\nContent-Length: 11\r\n\r\nsalutationsSOMEJUNK" in
  let str = Byte_stream.of_string q in
  let r = Request.Internal_.parse_req_start str in
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
  type body = [`String of string | `Stream of byte_stream | `Void]
  type t = {
    code: Response_code.t;
    headers: Headers.t;
    body: body;
  }

  let make_raw ?(headers=[]) ~code body : t =
    (* add content length to response *)
    let headers =
      Headers.set "Content-Length" (string_of_int (String.length body)) headers
    in
    { code; headers; body=`String body; }

  let make_raw_stream ?(headers=[]) ~code body : t =
    (* add content length to response *)
    let headers = Headers.set "Transfer-Encoding" "chunked" headers in
    { code; headers; body=`Stream body; }

  let make_void ?(headers=[]) ~code () : t =
    { code; headers; body=`Void; }

  let make_string ?headers r = match r with
    | Ok body -> make_raw ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?headers ~code msg

  let make_stream ?headers r = match r with
    | Ok body -> make_raw_stream ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?headers ~code msg

  let make ?headers r : t = match r with
    | Ok (`String body) -> make_raw ?headers ~code:200 body
    | Ok (`Stream body) -> make_raw_stream ?headers ~code:200 body
    | Ok `Void -> make_void ?headers ~code:200 ()
    | Error (code,msg) -> make_raw ?headers ~code msg

  let fail ?headers ~code fmt =
    Printf.ksprintf (fun msg -> make_raw ?headers ~code msg) fmt
  let fail_raise ~code fmt =
    Printf.ksprintf (fun msg -> raise (Bad_req (code,msg))) fmt

  let pp out self : unit =
    let pp_body out = function
      | `String s -> Format.fprintf out "%S" s
      | `Stream _ -> Format.pp_print_string out "<stream>"
      | `Void -> ()
    in
    Format.fprintf out "{@[code=%d;@ headers=[@[%a@]];@ body=%a@]}"
      self.code Headers.pp self.headers pp_body self.body

  (* print a stream as a series of chunks *)
  let output_stream_chunked_ (oc:out_channel) (str:byte_stream) : unit =
    let continue = ref true in
    while !continue do
      (* next chunk *)
      let s, i, len = str.bs_fill_buf () in
      Printf.fprintf oc "%x\r\n" len;
      output oc s i len;
      str.bs_consume len;
      if len = 0 then (
        continue := false;
      );
      output_string oc "\r\n";
    done;
    ()

  let output_ (oc:out_channel) (self:t) : unit =
    Printf.fprintf oc "HTTP/1.1 %d %s\r\n" self.code (Response_code.descr self.code);
    let body, is_chunked = match self.body with
      | `String s when String.length s > 1024 * 500 ->
        (* chunk-encode large bodies *)
        `Stream (Byte_stream.of_string s), true
      | `String _ as b -> b, false
      | `Stream _ as b -> b, true
      | `Void as b -> b, false
    in
    let headers =
      if is_chunked then (
        self.headers
        |> Headers.set "transfer-encoding" "chunked"
        |> Headers.remove "content-length"
      ) else self.headers
    in
    let self = {self with headers; body} in
    _debug (fun k->k "output response: %s"
               (Format.asprintf "%a" pp {self with body=`String "<…>"}));
    List.iter (fun (k,v) -> Printf.fprintf oc "%s: %s\r\n" k v) headers;
    output_string oc "\r\n";
    begin match body with
      | `String "" | `Void -> ()
      | `String s -> output_string oc s;
      | `Stream str -> output_stream_chunked_ oc str;
    end;
    flush oc
end

(* semaphore, for limiting concurrency. *)
module Sem_ = struct
  type t = {
    mutable n : int;
    mutex : Mutex.t;
    cond : Condition.t;
  }

  let create n =
    if n <= 0 then invalid_arg "Semaphore.create";
    { n; mutex=Mutex.create(); cond=Condition.create(); }

  let acquire m t =
    Mutex.lock t.mutex;
    while t.n < m do
      Condition.wait t.cond t.mutex;
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
    | Rest : {
        url_encoded: bool;
      } -> (string -> 'b, 'b) t
    | Compose: ('a, 'b) comp * ('b, 'c) t -> ('a, 'c) t

  let return = Fire
  let rest_of_path = Rest {url_encoded=false}
  let rest_of_path_urlencoded = Rest {url_encoded=true}
  let (@/) a b = Compose (a,b)
  let string = String
  let string_urlencoded = String_urlencoded
  let int = Int
  let exact (s:string) = Exact s

  let rec eval :
    type a b. path -> (a,b) t -> a -> b option =
    fun path route f ->
    begin match path, route with
      | [], Fire -> Some f
      | _, Fire -> None
      | _, Rest {url_encoded} ->
        let whole_path = String.concat "/" path in
        begin match
            if url_encoded
            then match Tiny_httpd_util.percent_decode whole_path with
              | Some s -> s
              | None -> raise_notrace Exit
            else whole_path
          with
          | whole_path ->
            Some (f whole_path)
          | exception Exit -> None
        end
      | (c1 :: path'), Compose (comp, route') ->
        begin match comp with
          | Int ->
            begin match int_of_string c1 with
              | i -> eval path' route' (f i)
              | exception _ -> None
            end
          | String ->
            eval path' route' (f c1)
          | String_urlencoded ->
            begin match Tiny_httpd_util.percent_decode c1 with
              | None -> None
              | Some s -> eval path' route' (f s)
            end
          | Exact s ->
            if s = c1 then eval path' route' f else None
        end
      | [], Compose (String, Fire) -> Some (f "") (* trailing *)
      | [], Compose (String_urlencoded, Fire) -> Some (f "") (* trailing *)
      | [], Compose _ -> None
    end

  let bpf = Printf.bprintf
  let rec pp_
    : type a b. Buffer.t -> (a,b) t -> unit
    = fun out -> function
      | Fire -> bpf out "/"
      | Rest {url_encoded} ->
        bpf out "<rest_of_url%s>" (if url_encoded then "_urlencoded" else "")
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

(* a request handler. handles a single request. *)
type cb_path_handler =
  out_channel ->
  byte_stream Request.t ->
  resp:(Response.t -> unit) ->
  unit

module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit
  val send_event :
    ?event:string ->
    ?id:string ->
    ?retry:string ->
    data:string ->
    unit -> unit
end
type server_sent_generator = (module SERVER_SENT_GENERATOR)

type t = {
  addr: string;

  port: int;

  sem_max_connections: Sem_.t;
  (* semaphore to restrict the number of active concurrent connections *)

  new_thread: (unit -> unit) -> unit;
  (* a function to run the given callback in a separate thread (or thread pool) *)

  masksigpipe: bool;

  mutable handler: (string Request.t -> Response.t);
  (* toplevel handler, if any *)

  mutable path_handlers : (unit Request.t -> cb_path_handler resp_result option) list;
  (* path handlers *)

  mutable cb_decode_req:
    (unit Request.t -> (unit Request.t * (byte_stream -> byte_stream)) option) list;
  (* middleware to decode requests *)

  mutable cb_encode_resp: (unit Request.t -> Response.t -> Response.t option) list;
  (* middleware to encode responses *)

  mutable running: bool;
  (* true while the server is running. no need to protect with a mutex,
     writes should be atomic enough. *)
}

let addr self = self.addr
let port self = self.port

let add_decode_request_cb self f =  self.cb_decode_req <- f :: self.cb_decode_req
let add_encode_response_cb self f = self.cb_encode_resp <- f :: self.cb_encode_resp
let set_top_handler self f = self.handler <- f

let add_path_handler_
    ?(accept=fun _req -> Ok ())
    ?meth ~tr_req self fmt f =
  let ph req : cb_path_handler resp_result option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      begin match Scanf.sscanf (Request.non_query_path req) fmt f with
        | handler ->
          (* we have a handler, do we accept the request based on its headers? *)
          begin match accept req with
            | Ok () -> Some (Ok (fun _oc req ~resp -> resp (handler (tr_req req))))
            | Error _ as e -> Some e
          end
        | exception _ ->
          None (* path didn't match *)
      end
  in
  self.path_handlers <- ph :: self.path_handlers

(* TODO: remove *)
let add_path_handler ?accept ?meth self fmt f =
  add_path_handler_ ?accept ?meth ~tr_req:Request.read_body_full self fmt f

(* TODO: remove *)
let add_path_handler_stream ?accept ?meth self fmt f =
  add_path_handler_ ?accept ?meth ~tr_req:(fun x->x) self fmt f

(* route the given handler.
   @param tr_req wraps the actual concrete function returned by the route
   and makes it into a handler. *)
let add_route_handler_
    ?(accept=fun _req -> Ok ())
    ?meth ~tr_req self (route:_ Route.t) f =
  let ph req : cb_path_handler resp_result option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      begin match Route.eval req.Request.path_components route f with
        | Some handler ->
          (* we have a handler, do we accept the request based on its headers? *)
          begin match accept req with
            | Ok () -> Some (Ok (fun oc req ~resp -> tr_req oc req ~resp handler))
            | Error _ as e -> Some e
          end
        | None ->
          None (* path didn't match *)
      end
  in
  self.path_handlers <- ph :: self.path_handlers

let add_route_handler (type a) ?accept ?meth self (route:(a,_) Route.t) (f:_) : unit =
  let tr_req _oc req ~resp f = resp (f (Request.read_body_full req)) in
  add_route_handler_ ?accept ?meth self route ~tr_req f

let add_route_handler_stream ?accept ?meth self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  add_route_handler_ ?accept ?meth self route ~tr_req f

let[@inline] _opt_iter ~f o = match o with
  | None -> ()
  | Some x -> f x

let add_route_server_sent_handler ?accept self route f =
  let tr_req oc req ~resp f =
    let req = Request.read_body_full req in
    let headers = ref Headers.(empty |> set "content-type" "text/event-stream") in

    (* send response once *)
    let resp_sent = ref false in
    let send_response_idempotent_ () =
      if not !resp_sent then (
        resp_sent := true;
        (* send 200 response now *)
        let initial_resp = Response.make_void ~headers:!headers ~code:200 () in
        resp initial_resp;
      )
    in

    let send_event ?event ?id ?retry ~data () : unit =
      send_response_idempotent_();
      _opt_iter event ~f:(fun e -> Printf.fprintf oc "data: %s\n" e);
      _opt_iter id ~f:(fun e -> Printf.fprintf oc "id: %s\n" e);
      _opt_iter retry ~f:(fun e -> Printf.fprintf oc "retry: %s\n" e);
      let l = String.split_on_char '\n' data in
      List.iter (fun s -> Printf.fprintf oc "data: %s\n" s) l;
      output_string oc "\n"; (* finish group *)
      flush oc
    in
    let module SSG = struct
      let set_headers h =
        if not !resp_sent then (
          headers := List.rev_append h !headers;
          send_response_idempotent_()
        )
      let send_event = send_event
    end in
    f req (module SSG : SERVER_SENT_GENERATOR);
  in
  add_route_handler_ self ?accept ~meth:`GET route ~tr_req f

let create
    ?(masksigpipe=true)
    ?(max_connections=32)
    ?(new_thread=(fun f -> ignore (Thread.create f () : Thread.t)))
    ?(addr="127.0.0.1") ?(port=8080) () : t =
  let handler _req = Response.fail ~code:404 "no top handler" in
  let max_connections = max 4 max_connections in
  { new_thread; addr; port; masksigpipe; handler;
    running= true; sem_max_connections=Sem_.create max_connections;
    path_handlers=[];
    cb_encode_resp=[]; cb_decode_req=[];
  }

let stop s = s.running <- false

let find_map f l =
  let rec aux f = function
    | [] -> None
    | x::l' ->
      match f x with
        | Some _ as res -> res
        | None -> aux f l'
  in aux f l

let handle_client_ (self:t) (client_sock:Unix.file_descr) : unit =
  let ic = Unix.in_channel_of_descr client_sock in
  let oc = Unix.out_channel_of_descr client_sock in
  let buf = Buf_.create() in
  let is = Byte_stream.of_chan ic in
  let continue = ref true in
  while !continue && self.running do
    _debug (fun k->k "read next request");
    match Request.parse_req_start ~buf is with
    | Ok None ->
      continue := false (* client is done *)

    | Error (c,s) ->
      (* connection error, close *)
      let res = Response.make_raw ~code:c s in
      begin
        try Response.output_ oc res
        with Sys_error _ -> ()
      end;
      continue := false

    | Ok (Some req) ->
      _debug (fun k->k "req: %s" (Format.asprintf "@[%a@]" Request.pp_ req));

      try
        (* is there a handler for this path? *)
        let handler =
          match find_map (fun ph -> ph req) self.path_handlers with
          | Some f -> unwrap_resp_result f
          | None -> (fun _oc req ~resp -> resp (self.handler (Request.read_body_full req)))
        in

        (* handle expect/continue *)
        begin match Request.get_header ~f:String.trim req "Expect" with
          | Some "100-continue" ->
            _debug (fun k->k "send back: 100 CONTINUE");
            Response.output_ oc (Response.make_raw ~code:100 "");
          | Some s -> bad_reqf 417 "unknown expectation %s" s
          | None -> ()
        end;

        (* preprocess request's input stream *)
        let req0, tr_stream =
          List.fold_left
            (fun (req,tr) cb ->
               match cb req with
               | None -> req, tr
               | Some (r',f) -> r', (fun is -> tr is |> f))
            (req, (fun is->is)) self.cb_decode_req
        in
        (* now actually read request's body into a stream *)
        let req =
          Request.parse_body_ ~tr_stream ~buf {req0 with body=is}
          |> unwrap_resp_result
        in

        (* how to post-process response accordingly *)
        let post_process_resp resp =
          List.fold_left
            (fun resp cb -> match cb req0 resp with None -> resp | Some r' -> r')
            resp self.cb_encode_resp
        in

        (* how to reply *)
        let resp r =
          try
            let r = post_process_resp r in
            Response.output_ oc r
          with Sys_error _ -> continue := false
        in

        (* call handler *)
        begin
          try handler oc req ~resp
          with Sys_error _ -> continue := false
        end
      with
      | Sys_error _ ->
        continue := false; (* connection broken somehow *)
      | Bad_req (code,s) ->
        continue := false;
        Response.output_ oc @@ Response.make_raw ~code s
      | e ->
        continue := false;
        Response.output_ oc @@ Response.fail ~code:500 "server error: %s" (Printexc.to_string e)
  done;
  _debug (fun k->k "done with client, exiting");
  (try Unix.close client_sock
   with e -> _debug (fun k->k "error when closing sock: %s" (Printexc.to_string e)));
  ()

let is_ipv6 self = String.contains self.addr ':'

let run (self:t) : (unit,_) result =
  try
    if self.masksigpipe then (
      ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
    );
    let sock =
      Unix.socket (if is_ipv6 self then Unix.PF_INET6 else Unix.PF_INET)
        Unix.SOCK_STREAM 0
    in
    Unix.clear_nonblock sock;
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.setsockopt_optint sock Unix.SO_LINGER None;
    let inet_addr = Unix.inet_addr_of_string self.addr in
    Unix.bind sock (Unix.ADDR_INET (inet_addr, self.port));
    Unix.listen sock (2 * self.sem_max_connections.Sem_.n);
    while self.running do
      (* limit concurrency *)
      Sem_.acquire 1 self.sem_max_connections;
      let client_sock, _ = Unix.accept sock in
      self.new_thread
        (fun () ->
           try
             handle_client_ self client_sock;
             Sem_.release 1 self.sem_max_connections;
           with e ->
             (try Unix.close client_sock with _ -> ());
             Sem_.release 1 self.sem_max_connections;
             raise e
        );
    done;
    Ok ()
  with e -> Error e
