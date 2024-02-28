open Common_

type 'body t = {
  meth: Meth.t;
  host: string;
  client_addr: Unix.sockaddr;
  headers: Headers.t;
  mutable meta: Hmap.t;
  http_version: int * int;
  path: string;
  path_components: string list;
  query: (string * string) list;
  body: 'body;
  start_time: float;
}

let headers self = self.headers
let host self = self.host
let client_addr self = self.client_addr
let meth self = self.meth
let path self = self.path
let body self = self.body
let start_time self = self.start_time
let query self = self.query
let get_header ?f self h = Headers.get ?f h self.headers
let remove_header k self = { self with headers = Headers.remove k self.headers }
let add_meta self k v = self.meta <- Hmap.add k v self.meta
let get_meta self k = Hmap.find k self.meta
let get_meta_exn self k = Hmap.get k self.meta

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
    (String.concat ";" @@ List.map (fun (a, b) -> Printf.sprintf "%S,%S" a b) q)

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
let read_stream_chunked_ ~bytes (bs : #IO.Input.t) : IO.Input.t =
  Log.debug (fun k -> k "body: start reading chunked stream...");
  IO.Input.read_chunked ~bytes ~fail:(fun s -> Bad_req (400, s)) bs

let limit_body_size_ ~max_size ~bytes (bs : #IO.Input.t) : IO.Input.t =
  Log.debug (fun k -> k "limit size of body to max-size=%d" max_size);
  IO.Input.limit_size_to ~max_size ~close_rec:false ~bytes bs

let limit_body_size ~max_size ~bytes (req : IO.Input.t t) : IO.Input.t t =
  { req with body = limit_body_size_ ~max_size ~bytes req.body }

(** read exactly [size] bytes from the stream *)
let read_exactly ~size ~bytes (bs : #IO.Input.t) : IO.Input.t =
  Log.debug (fun k -> k "body: must read exactly %d bytes" size);
  IO.Input.reading_exactly bs ~close_rec:false ~bytes ~size

(* parse request, but not body (yet) *)
let parse_req_start ~client_addr ~get_time_s ~buf (bs : IO.Input.t) :
    unit t option resp_result =
  try
    let line = IO.Input.read_line_using ~buf bs in
    Log.debug (fun k -> k "parse request line: %s" line);
    let start_time = get_time_s () in
    let meth, path, version =
      try
        let off = ref 0 in
        let meth = Parse_.word line off in
        let path = Parse_.word line off in
        let http_version = Parse_.word line off in
        let version =
          match http_version with
          | "HTTP/1.1" -> 1
          | "HTTP/1.0" -> 0
          | v -> invalid_arg (spf "unsupported HTTP version: %s" v)
        in
        meth, path, version
      with
      | Invalid_argument msg ->
        Log.error (fun k -> k "invalid request line: `%s`: %s" line msg);
        raise (Bad_req (400, "Invalid request line"))
      | _ ->
        Log.error (fun k -> k "invalid request line: `%s`" line);
        raise (Bad_req (400, "Invalid request line"))
    in
    let meth = Meth.of_string meth in
    Log.debug (fun k -> k "got meth: %s, path %S" (Meth.to_string meth) path);
    let headers = Headers.parse_ ~buf bs in
    let host =
      match Headers.get "Host" headers with
      | None -> bad_reqf 400 "No 'Host' header in request"
      | Some h -> h
    in
    let path_components, query = Util.split_query path in
    let path_components = Util.split_on_slash path_components in
    let query =
      match Util.parse_query query with
      | Ok l -> l
      | Error e -> bad_reqf 400 "invalid query: %s" e
    in
    let req =
      {
        meth;
        query;
        host;
        meta = Hmap.empty;
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
let parse_body_ ~tr_stream ~bytes (req : IO.Input.t t) :
    IO.Input.t t resp_result =
  try
    let size, has_size =
      match Headers.get_exn "Content-Length" req.headers |> int_of_string with
      | n -> n, true (* body of fixed size *)
      | exception Not_found -> 0, false
      | exception _ -> bad_reqf 400 "invalid content-length"
    in
    let body =
      match get_header ~f:String.trim req "Transfer-Encoding" with
      | None -> read_exactly ~size ~bytes @@ tr_stream req.body
      | Some "chunked" when has_size ->
        bad_reqf 400 "specifying both transfer-encoding and content-length"
      | Some "chunked" ->
        (* body sent by chunks *)
        let bs : IO.Input.t =
          read_stream_chunked_ ~bytes @@ tr_stream req.body
        in
        if size > 0 then (
          (* TODO: ensure we recycle [bytes] when the new input is closed *)
          let bytes = Bytes.create 4096 in
          limit_body_size_ ~max_size:size ~bytes bs
        ) else
          bs
      | Some s -> bad_reqf 500 "cannot handle transfer encoding: %s" s
    in
    Ok { req with body }
  with
  | End_of_file -> Error (400, "unexpected end of file")
  | Bad_req (c, s) -> Error (c, s)
  | e -> Error (400, Printexc.to_string e)

let read_body_full ?bytes ?buf_size (self : IO.Input.t t) : string t =
  try
    let buf =
      match bytes with
      | Some b -> Buf.of_bytes b
      | None -> Buf.create ?size:buf_size ()
    in
    let body = IO.Input.read_all_using ~buf self.body in
    { self with body }
  with
  | Bad_req _ as e -> raise e
  | e -> bad_reqf 500 "failed to read body: %s" (Printexc.to_string e)

module Private_ = struct
  let close_after_req = close_after_req
  let parse_req_start = parse_req_start

  let parse_req_start_exn ?(buf = Buf.create ()) ~client_addr ~get_time_s bs =
    parse_req_start ~client_addr ~get_time_s ~buf bs |> unwrap_resp_result

  let parse_body ?(bytes = Bytes.create 4096) req bs : _ t =
    parse_body_ ~tr_stream:(fun s -> s) ~bytes { req with body = bs }
    |> unwrap_resp_result

  let[@inline] set_body body self = { self with body }
end
