
exception Bad_req of int * string
let bad_reqf c fmt = Printf.ksprintf (fun s ->raise (Bad_req (c,s))) fmt

module Meth = struct
  type t = [
    | `GET
    | `PUT
    | `POST
    | `HEAD
  ]

  let to_string = function
    | `GET -> "GET"
    | `PUT -> "PUT"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
  let pp out s = Format.pp_print_string out (to_string s)

  let of_string = function
    | "GET" -> `GET
    | "PUT" -> `PUT
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | s -> bad_reqf 400 "unknown method %S" s
end

module Headers = struct
  type t = (string * string) list
  let pp out l =
    let pp_pair out (k,v) = Format.fprintf out "@[<h>%s: %s@]" k v in
    Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l

  let parse_ (ic:in_channel) : t =
    let rec loop acc =
      let line = input_line ic in
      if line = "\r" then (
        List.rev acc
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

module Response_code = struct
  type t = int

  let descr = function
    | 200 -> "OK"
    | 400 -> "Bad request"
    | 403 -> "Forbidden"
    | 404 -> "Not found"
    | 500 -> "Internal server error"
    | 503 -> "Service unavailable"
    | _ -> "Unknown response" (* TODO *)
end

module Request = struct
  type t = {
    meth: Meth.t;
    headers: Headers.t;
    path: string;
    body: string
  }

  let headers self = self.headers
  let meth self = self.meth
  let path self = self.path
  let body self = self.body

  let pp out self : unit =
    Format.fprintf out "{@[meth=%s;@ headers=%a;@ path=%S;@ body=%S@]}"
      (Meth.to_string self.meth) Headers.pp self.headers
      self.path self.body

  let read_body ic (n:int) : string =
    let buf = Bytes.make n ' ' in
    let i = ref 0 in
    while !i < n do
      let read = input ic buf !i (n- !i) in
      if read=0 then bad_reqf 400 "body is too short";
      i := !i + read
    done;
    Bytes.unsafe_to_string buf

  let parse_ (ic:in_channel) : (t option, Response_code.t * string) result =
    try
      let line = input_line ic in
      let meth, path =
        try Scanf.sscanf line "%s %s HTTP/1.1\r" (fun x y->x,y)
        with _ -> raise (Bad_req (400, "Invalid request line"))
      in
      let meth = Meth.of_string meth in
      let headers = Headers.parse_ ic in
      let body = match List.assoc "Content-Length" headers |> int_of_string with
        | exception Not_found -> ""
        | exception _ -> bad_reqf 400 "invalid content-length"
        | 0 -> ""
        | n -> read_body ic n
      in
      Ok (Some {meth; path; body; headers})
    with
    | End_of_file -> Ok None
    | Bad_req (c,s) -> Error (c,s)
    | e ->
      Error (400, Printexc.to_string e)
end

module Response = struct
  type t = {
    code: Response_code.t;
    headers: Headers.t;
    body: string;
  }

  let make ?(headers=[]) ~code body : t =
    (* add 'content length' to response *)
    let headers = List.filter (fun (k,_) -> k <> "Content-Length") headers in
    let headers = ("Content-Length", string_of_int (String.length body)) :: headers in
    { code; headers; body; }

  let make_ok ?headers body = make ~code:200 ?headers body
  let make_not_found ?headers body = make ~code:404 ?headers body
  let make_error ?headers body = make ~code:500 ?headers body

  let pp out self : unit =
    Format.fprintf out "{@[code=%d;@ headers=%a;@ body=%S@]}"
      self.code Headers.pp self.headers self.body

  let output_ (oc:out_channel) (self:t) : unit =
    Printf.fprintf oc "HTTP/1.1 %d %s\r\n" self.code (Response_code.descr self.code);
    List.iter (fun (k,v) -> Printf.fprintf oc "%s: %s\r\n" k v) self.headers;
    Printf.fprintf oc "\r\n";
    if self.body<>"" then (
      output_string oc self.body;
    );
    flush oc
end

type t = {
  addr: string;
  port: int;
  fork: (unit -> unit) -> unit;
  masksigpipe: bool;
  mutable handler: (Request.t -> Response.t);
  mutable path_handlers : (Request.t -> (unit -> Response.t) option) list;
  mutable running: bool;
}

let addr self = self.addr
let port self = self.port

let set_top_handler self f = self.handler <- f

let add_path_handler ?meth self fmt f =
  let ph req: (unit -> Response.t) option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      try Some (Scanf.sscanf req.Request.path fmt (f req))
      with _ -> None
  in
  self.path_handlers <- ph :: self.path_handlers

let create
    ?(masksigpipe=true)
    ?(fork=(fun f -> ignore (Thread.create f () : Thread.t)))
    ?(addr="127.0.0.1") ?(port=8080) () : t =
  let handler _req = Response.make_not_found "no top handler" in
  { fork; addr; port; masksigpipe; handler; running= true;
    path_handlers=[]; }

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
  let handler = self.handler in
  let ph_handlers = self.path_handlers in
  let continue = ref true in
  while !continue && self.running do
    match Request.parse_ ic with
    | Ok None -> continue := false
    | Error (c,s) ->
      let res = Response.make ~code:c s in
      Response.output_ oc res
    | Ok (Some req) ->
      let res =
        let run_handler =
          match find_map (fun ph -> ph req) ph_handlers with
          | Some f -> f
          | None -> (fun () -> handler req)
        in
        try run_handler()
        with
        | e ->
          Response.make ~code:500 ("server error: " ^ Printexc.to_string e)
      in
      Response.output_ oc res
    | exception Sys_error _ ->
      continue := false; (* connection broken somehow *)
      Unix.close client_sock;
  done

let run (self:t) : (unit,_) result =
  try
    if self.masksigpipe then (
      ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
    );
    let sock = Unix.socket ~cloexec:true PF_INET Unix.SOCK_STREAM 0 in
    let inet_addr = Unix.inet_addr_of_string self.addr in
    Unix.bind sock (Unix.ADDR_INET (inet_addr, self.port));
    Unix.listen sock 10;
    while self.running do
      let client_sock, _ = Unix.accept sock in
      self.fork
        (fun () -> handle_client_ self client_sock);
    done;
    Ok ()
  with e -> Error e
