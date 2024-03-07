open Common_

type body =
  [ `String of string | `Stream of IO.Input.t | `Writer of IO.Writer.t | `Void ]

type t = { code: Response_code.t; headers: Headers.t; body: body }

let set_body body self = { self with body }
let set_headers headers self = { self with headers }
let update_headers f self = { self with headers = f self.headers }
let set_header k v self = { self with headers = Headers.set k v self.headers }
let remove_header k self = { self with headers = Headers.remove k self.headers }
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

let make_string ?headers ?(code = 200) r =
  match r with
  | Ok body -> make_raw ?headers ~code body
  | Error (code, msg) -> make_raw ?headers ~code msg

let make_stream ?headers ?(code = 200) r =
  match r with
  | Ok body -> make_raw_stream ?headers ~code body
  | Error (code, msg) -> make_raw ?headers ~code msg

let make_writer ?headers ?(code = 200) r : t =
  match r with
  | Ok body -> make_raw_writer ?headers ~code body
  | Error (code, msg) -> make_raw ?headers ~code msg

let make ?headers ?(code = 200) r : t =
  match r with
  | Ok (`String body) -> make_raw ?headers ~code body
  | Ok (`Stream body) -> make_raw_stream ?headers ~code body
  | Ok `Void -> make_void ?headers ~code ()
  | Ok (`Writer f) -> make_raw_writer ?headers ~code f
  | Error (code, msg) -> make_raw ?headers ~code msg

let fail ?headers ~code fmt =
  Printf.ksprintf (fun msg -> make_raw ?headers ~code msg) fmt

exception Bad_req = Bad_req

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

let output_ ~bytes (oc : IO.Output.t) (self : t) : unit =
  (* double indirection:
     - print into [buffer] using [bprintf]
     - transfer to [buf_] so we can output from there *)
  let tmp_buffer = Buffer.create 32 in
  let buf = Buf.of_bytes bytes in

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
  Log.debug (fun k ->
      k "t[%d]: output response: %s"
        (Thread.id @@ Thread.self ())
        (Format.asprintf "%a" pp { self with body = `String "<...>" }));

  (* write headers, using [buf] to batch writes *)
  List.iter
    (fun (k, v) ->
      Printf.bprintf tmp_buffer "%s: %s\r\n" k v;
      Buf.add_buffer buf tmp_buffer;
      Buffer.clear tmp_buffer)
    headers;

  IO.Output.output_buf oc buf;
  IO.Output.output_string oc "\r\n";
  Buf.clear buf;

  (match body with
  | `String "" | `Void -> ()
  | `String s -> IO.Output.output_string oc s
  | `Writer w ->
    (* use buffer to chunk encode [w] *)
    let oc' = IO.Output.chunk_encoding ~buf ~close_rec:false oc in
    (try
       IO.Writer.write oc' w;
       IO.Output.close oc'
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       IO.Output.close oc';
       IO.Output.flush oc;
       Printexc.raise_with_backtrace e bt)
  | `Stream str ->
    (match IO.Input.output_chunked' ~buf oc str with
    | () ->
      Log.debug (fun k ->
          k "t[%d]: done outputing stream" (Thread.id @@ Thread.self ()));
      IO.Input.close str
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Log.error (fun k ->
          k "t[%d]: outputing stream failed with %s"
            (Thread.id @@ Thread.self ())
            (Printexc.to_string e));
      IO.Input.close str;
      IO.Output.flush oc;
      Printexc.raise_with_backtrace e bt));
  IO.Output.flush oc

module Private_ = struct
  let make_void_force_ = make_void_force_
  let output_ = output_
end
