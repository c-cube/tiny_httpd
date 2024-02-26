module W = IO.Writer

(* TODO: just use iostream-camlzip? *)

let decode_deflate_stream_ ~buf_size (ic : IO.Input.t) : IO.Input.t =
  Log.debug (fun k -> k "wrap stream with deflate.decode");
  Iostream_camlzip.decompress_in_buf ~buf_size ic

let encode_deflate_writer_ ~buf_size (w : W.t) : W.t =
  Log.debug (fun k -> k "wrap writer with deflate.encode");

  let { IO.Writer.write } = w in
  let write' (oc : IO.Output.t) =
    let oc' = Iostream_camlzip.compressed_out ~buf_size ~level:4 oc in
    write (oc' :> IO.Output.t)
  in
  IO.Writer.make ~write:write' ()

let accept_deflate (req : _ Request.t) =
  match Request.get_header req "Accept-Encoding" with
  | Some s ->
    List.mem "deflate" @@ List.rev_map String.trim @@ String.split_on_char ',' s
  | None -> false

let has_deflate s =
  try Scanf.sscanf s "deflate, %s" (fun _ -> true) with _ -> false

(* decompress [req]'s body if needed *)
let decompress_req_stream_ ~buf_size (req : IO.Input.t Request.t) : _ Request.t
    =
  match Request.get_header ~f:String.trim req "Transfer-Encoding" with
  (* TODO
     | Some "gzip" ->
       let req' = S.Request.set_header req "Transfer-Encoding" "chunked" in
       Some (req', decode_gzip_stream_)
  *)
  | Some "deflate" ->
    let body' = Request.body req |> decode_deflate_stream_ ~buf_size in
    req |> Request.remove_header "Transfer-Encoding" |> Request.set_body body'
  | Some s when has_deflate s ->
    (match Scanf.sscanf s "deflate, %s" (fun s -> s) with
    | tr' ->
      let body' = Request.body req |> decode_deflate_stream_ ~buf_size in
      req
      |> Request.set_header "Transfer-Encoding" tr'
      |> Request.set_body body'
    | exception _ -> req)
  | _ -> req

let compress_resp_stream_ ~compress_above ~buf_size (req : _ Request.t)
    (resp : Response.t) : Response.t =
  (* headers for compressed stream *)
  let update_headers h =
    h
    |> Headers.remove "Content-Length"
    |> Headers.set "Content-Encoding" "deflate"
  in

  if accept_deflate req then (
    match resp.body with
    | `String s when String.length s > compress_above ->
      (* big string, we compress *)
      Log.debug (fun k ->
          k "encode str response with deflate (size %d, threshold %d)"
            (String.length s) compress_above);
      let body = encode_deflate_writer_ ~buf_size @@ W.of_string s in
      resp
      |> Response.update_headers update_headers
      |> Response.set_body (`Writer body)
    | `Stream ic ->
      Log.debug (fun k -> k "encode stream response with deflate");
      let w = IO.Writer.of_input ic in
      resp
      |> Response.update_headers update_headers
      |> Response.set_body (`Writer (encode_deflate_writer_ ~buf_size w))
    | `Writer w ->
      Log.debug (fun k -> k "encode writer response with deflate");
      resp
      |> Response.update_headers update_headers
      |> Response.set_body (`Writer (encode_deflate_writer_ ~buf_size w))
    | `String _ | `Void -> resp
  ) else
    resp

let middleware ?(compress_above = 16 * 1024) ?(buf_size = 16 * 1_024) () :
    Server.Middleware.t =
  let buf_size = max buf_size 1_024 in
  fun h req ~resp ->
    let req = decompress_req_stream_ ~buf_size req in
    h req ~resp:(fun response ->
        resp @@ compress_resp_stream_ ~buf_size ~compress_above req response)

let setup ?compress_above ?buf_size server =
  let m = middleware ?compress_above ?buf_size () in
  Log.info (fun k -> k "setup gzip middleware");
  Server.add_middleware ~stage:`Encoding server m
