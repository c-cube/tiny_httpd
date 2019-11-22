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
       Printf.fprintf stdout "[thread %d]: " Thread.(id @@ self());
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
      resize self (self.i + len + 10);
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

  let of_bytes ?(i=0) ?len s : t =
    let len =
      ref (
        match len with
        | Some n -> min n (Bytes.length s- i)
        | None -> Bytes.length s- i
      )
    in
    let i = ref i in
    { bs_fill_buf=(fun () -> s, !i, !len);
      bs_close=(fun () -> ());
      bs_consume=(fun n -> i := !i + n; len := !len - n);
    }

  let with_file file f =
    let ic = open_in file in
    try
      let x = f (of_chan ic) in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  (* Read as much as possible into [buf]. *)
  let read_into_buf (self:t) (buf:Buf_.t) : int =
    let s, i, len = self.bs_fill_buf () in
     if len > 0 then (
       Buf_.add_bytes buf s i len;
       self.bs_consume len;
     );
     len

  let read_all ?(buf=Buf_.create()) (self:t) : string =
    let continue = ref true in
    while !continue do
      let n_rd = read_into_buf self buf in
      if n_rd = 0 then (
        continue := false
      )
    done;
    Buf_.contents_and_clear buf

  (* put [n] bytes from the input into bytes *)
  let read_exactly_ ~too_short (self:t) (bytes:bytes) (n:int) : unit =
    assert (Bytes.length bytes >= n);
    let offset = ref 0 in
    while !offset < n do
      let n_read =
        let s, i, len = self.bs_fill_buf () in
        let n_read = min len (n- !offset) in
        Bytes.blit s i bytes !offset n_read;
        offset := !offset + n_read;
        self.bs_consume n_read;
        n_read
      in
      if n_read=0 then too_short();
    done

  (* read a line into the buffer *)
  let read_line_into (self:t) ~buf : unit =
    Buf_.clear buf;
    let continue = ref true in
    while !continue do
      let s, i, len = self.bs_fill_buf () in
      if len=0 then continue := false;
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
  let contains = List.mem_assoc
  let get ?(f=fun x->x) x h = try Some (List.assoc x h |> f) with Not_found -> None
  let remove x h = List.filter (fun (k,_) -> k<>x) h
  let set x y h = (x,y) :: List.filter (fun (k,_) -> k<>x) h
  let pp out l =
    let pp_pair out (k,v) = Format.fprintf out "@[<h>%s: %s@]" k v in
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l

  let parse_ ~buf (bs:byte_stream) : t =
    let rec loop acc =
      let line = Byte_stream.read_line ~buf bs in
      _debug (fun k->k  "parsed header line %S" line);
      if line = "\r" then (
        acc
      ) else (
        let k,v =
          try Scanf.sscanf line "%s@: %s@\r" (fun k v->k,v)
          with _ -> bad_reqf 400 "invalid header line: %S" line
        in
        loop ((k,v)::acc)
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
    body: 'body;
  }

  let headers self = self.headers
  let host self = self.host
  let meth self = self.meth
  let path self = self.path
  let body self = self.body

  let get_header ?f self h = Headers.get ?f h self.headers
  let get_header_int self h = match get_header self h with
    | Some x -> (try Some (int_of_string x) with _ -> None)
    | None -> None
  let set_header self k v = {self with headers=Headers.set k v self.headers}

  let pp_ out self : unit =
    Format.fprintf out "{@[meth=%s;@ host=%s;@ headers=%a;@ path=%S;@ body=?@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers self.path
  let pp out self : unit =
    Format.fprintf out "{@[meth=%s;@ host=%s;@ headers=%a;@ path=%S;@ body=%S@]}"
      (Meth.to_string self.meth) self.host Headers.pp self.headers
      self.path self.body

  let read_body_exact (bs:byte_stream) (n:int) : string =
    let bytes = Bytes.make n ' ' in
    Byte_stream.read_exactly_ bs bytes n
      ~too_short:(fun () -> bad_reqf 400 "body is too short");
    Bytes.unsafe_to_string bytes

  (* decode a "chunked" stream into a normal stream *)
  let read_stream_chunked_ ?(buf=Buf_.create()) (bs:byte_stream) : byte_stream =
    let read_next_chunk_len () : int =
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
    let bytes = Bytes.make 4096  ' ' in
    let offset = ref 0 in
    let len = ref 0 in
    let chunk_size = ref 0 in
    { bs_fill_buf=
        (fun () ->
           (* do we need to refill? *)
           if !offset >= !len then (
             if !chunk_size = 0 && !refill then (
               chunk_size := read_next_chunk_len();
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
      bs_close=(fun () -> Byte_stream.close bs);
    }

  let read_body_chunked ~tr_stream ~buf ~size:max_size (bs:byte_stream) : string =
    _debug (fun k->k "read body with chunked encoding (max-size: %d)" max_size);
    let is = tr_stream @@ read_stream_chunked_ ~buf bs in
    let buf_res = Buf_.create() in (* store the accumulated chunks *)
    (* TODO: extract this as a function [read_all_up_to ~max_size is]? *)
    let rec read_chunks () =
      let n = Byte_stream.read_into_buf is buf_res in
      if n = 0 then (
        Buf_.contents buf_res (* done *)
      ) else (
        (* is the body bigger than expected? *)
        if max_size>0 && Buf_.size buf_res > max_size then (
          bad_reqf 413
            "body size was supposed to be %d, but at least %d bytes received"
            max_size (Buf_.size buf_res)
        );
        read_chunks()
      )
    in
    read_chunks()

  (* parse request, but not body (yet) *)
  let parse_req_start ~buf (bs:byte_stream) : unit t option resp_result =
    try
      let line = Byte_stream.read_line ~buf bs in
      let meth, path =
        try Scanf.sscanf line "%s %s HTTP/1.1\r" (fun x y->x,y)
        with _ -> raise (Bad_req (400, "Invalid request line"))
      in
      let meth = Meth.of_string meth in
      _debug (fun k->k "got meth: %s, path %S" (Meth.to_string meth) path);
      let headers = Headers.parse_ ~buf bs in
      let host =
        try List.assoc "Host" headers
        with Not_found -> bad_reqf 400 "No 'Host' header in request"
      in
      Ok (Some {meth; host; path; headers; body=()})
    with
    | End_of_file | Sys_error _ -> Ok None
    | Bad_req (c,s) -> Error (c,s)
    | e ->
      Error (400, Printexc.to_string e)

  (* parse body, given the headers.
     @param tr_stream a transformation of the input stream. *)
  let parse_body_ ~tr_stream ~buf (req:byte_stream t) : string t resp_result =
    try
      let size =
        match List.assoc "Content-Length" req.headers |> int_of_string with
        | n -> n (* body of fixed size *)
        | exception Not_found -> 0
        | exception _ -> bad_reqf 400 "invalid content-length"
      in
      let body =
        match get_header ~f:String.trim req "Transfer-Encoding" with
        | None -> read_body_exact (tr_stream req.body) size
        | Some "chunked" ->
          read_body_chunked ~tr_stream ~buf ~size req.body (* body sent by chunks *)
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
end

module Response = struct
  type body = [`String of string | `Stream of byte_stream]
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

  let make_string ?headers r = match r with
    | Ok body -> make_raw ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?headers ~code msg

  let make_stream ?headers r = match r with
    | Ok body -> make_raw_stream ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?headers ~code msg

  let make ?headers r : t = match r with
    | Ok (`String body) -> make_raw ?headers ~code:200 body
    | Ok (`Stream body) -> make_raw_stream ?headers ~code:200 body
    | Error (code,msg) -> make_raw ?headers ~code msg

  let fail ?headers ~code fmt =
    Printf.ksprintf (fun msg -> make_raw ?headers ~code msg) fmt
  let fail_raise ~code fmt =
    Printf.ksprintf (fun msg -> raise (Bad_req (code,msg))) fmt

  let pp out self : unit =
    let pp_body out = function
      | `String s -> Format.fprintf out "%S" s
      | `Stream _ -> Format.pp_print_string out "<stream>"
    in
    Format.fprintf out "{@[code=%d;@ headers=%a;@ body=%a@]}"
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
    List.iter (fun (k,v) -> Printf.fprintf oc "%s: %s\r\n" k v) self.headers;
    Printf.fprintf oc "\r\n";
    begin match self.body with
      | `String "" -> ()
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

type cb_path_handler = string Request.t -> Response.t

type t = {
  addr: string;
  port: int;
  sem_max_connections: Sem_.t;
  new_thread: (unit -> unit) -> unit;
  masksigpipe: bool;
  mutable handler: (string Request.t -> Response.t);
  mutable path_handlers : (unit Request.t -> cb_path_handler resp_result option) list;
  mutable cb_decode_req:
    (unit Request.t -> (unit Request.t * (byte_stream -> byte_stream)) option) list;
  mutable cb_encode_resp: (string Request.t -> Response.t -> Response.t option) list;
  mutable running: bool;
}

let addr self = self.addr
let port self = self.port

let add_decode_request_cb self f =  self.cb_decode_req <- f :: self.cb_decode_req
let add_encode_response_cb self f = self.cb_encode_resp <- f :: self.cb_encode_resp
let set_top_handler self f = self.handler <- f

let add_path_handler
    ?(accept=fun _req -> Ok ())
    ?meth self fmt f =
  let ph req: cb_path_handler resp_result option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      begin match Scanf.sscanf req.Request.path fmt f with
        | handler ->
          (* we have a handler, do we accept the request based on its headers? *)
          begin match accept req with
            | Ok () -> Some (Ok handler)
            | Error _ as e -> Some e
          end
        | exception _ ->
          None (* path didn't match *)
      end
  in
  self.path_handlers <- ph :: self.path_handlers

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
    | Ok None -> continue := false
    | Error (c,s) ->
      let res = Response.make_raw ~code:c s in
      begin
        try Response.output_ oc res
        with Sys_error _ -> continue := false
      end
    | Ok (Some req) ->
      let res =
        try
          (* is there a handler for this path? *)
          let handler =
            match find_map (fun ph -> ph req) self.path_handlers with
            | Some f -> unwrap_resp_result f
            | None -> self.handler
          in
          (* handle expectations *)
          begin match Request.get_header ~f:String.trim req "Expect" with
            | Some "100-continue" ->
              _debug (fun k->k "send back: 100 CONTINUE");
              Response.output_ oc (Response.make_raw ~code:100 "");
            | Some s -> bad_reqf 417 "unknown expectation %s" s
            | None -> ()
          end;
          (* preprocess request's input stream *)
          let req, tr_stream =
            List.fold_left
              (fun (req,tr) cb ->
                 match cb req with
                 | None -> req, tr
                 | Some (r',f) -> r', (fun is -> tr is |> f))
              (req, (fun is->is)) self.cb_decode_req
          in
          (* now actually read request's body *)
          let req =
            Request.parse_body_ ~tr_stream ~buf {req with body=is}
            |> unwrap_resp_result
          in
          let resp = handler req in
          (* post-process response *)
          List.fold_left
            (fun resp cb -> match cb req resp with None -> resp | Some r' -> r')
            resp self.cb_encode_resp
        with
        | Bad_req (code,s) ->
          continue := false;
          Response.make_raw ~code s
        | e ->
          Response.fail ~code:500 "server error: %s" (Printexc.to_string e)
      in
      begin
        try Response.output_ oc res
        with Sys_error _ -> continue := false
      end
    | exception Bad_req (code,s) ->
      Response.output_ oc (Response.make_raw ~code s);
      continue := false
    | exception Sys_error _ ->
      continue := false; (* connection broken somehow *)
      Unix.close client_sock;
  done;
  _debug (fun k->k "done with client, exiting");
  (try Unix.close client_sock with _ -> ());
  ()

let run (self:t) : (unit,_) result =
  try
    if self.masksigpipe then (
      ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
    );
    let sock = Unix.socket PF_INET Unix.SOCK_STREAM 0 in
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
