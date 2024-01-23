module S = Tiny_httpd_server
module BS = Tiny_httpd_stream
module W = Tiny_httpd_io.Writer
module Out = Tiny_httpd_io.Output
module Log = Tiny_httpd.Log

let decode_deflate_stream_ ~buf_size (is : S.byte_stream) : S.byte_stream =
  Log.debug (fun k -> k "wrap stream with deflate.decode");
  let zlib_str = Zlib.inflate_init false in
  let is_done = ref false in
  BS.make ~bs:(Bytes.create buf_size)
    ~close:(fun _ ->
      Zlib.inflate_end zlib_str;
      BS.close is)
    ~consume:(fun self len ->
      if len > self.len then
        S.Response.fail_raise ~code:400
          "inflate: error during decompression: invalid consume len %d (max %d)"
          len self.len;
      self.off <- self.off + len;
      self.len <- self.len - len)
    ~fill:(fun self ->
      (* refill [buf] if needed *)
      if self.len = 0 && not !is_done then (
        is.fill_buf ();
        (try
           let finished, used_in, used_out =
             Zlib.inflate zlib_str self.bs 0 (Bytes.length self.bs) is.bs is.off
               is.len Zlib.Z_SYNC_FLUSH
           in
           is.consume used_in;
           self.off <- 0;
           self.len <- used_out;
           if finished then is_done := true;
           Log.debug (fun k ->
               k "decode %d bytes as %d bytes from inflate (finished: %b)"
                 used_in used_out finished)
         with Zlib.Error (e1, e2) ->
           S.Response.fail_raise ~code:400
             "inflate: error during decompression:\n%s %s" e1 e2);
        Log.debug (fun k ->
            k "inflate: refill %d bytes into internal buf" self.len)
      ))
    ()

let encode_deflate_writer_ ~buf_size (w : W.t) : W.t =
  Log.debug (fun k -> k "wrap writer with deflate.encode");
  let zlib_str = Zlib.deflate_init 4 false in

  let o_buf = Bytes.create buf_size in
  let o_off = ref 0 in
  let o_len = ref 0 in

  (* write output buffer to out *)
  let write_out (oc : Out.t) =
    if !o_len > 0 then (
      Out.output oc o_buf !o_off !o_len;
      o_off := 0;
      o_len := 0
    )
  in

  let flush_zlib ~flush (oc : Out.t) =
    let continue = ref true in
    while !continue do
      let finished, used_in, used_out =
        Zlib.deflate zlib_str Bytes.empty 0 0 o_buf 0 (Bytes.length o_buf) flush
      in
      assert (used_in = 0);
      o_len := !o_len + used_out;
      if finished then continue := false;
      write_out oc
    done
  in

  (* compress and consume input buffer *)
  let write_zlib ~flush (oc : Out.t) buf i len =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      let _finished, used_in, used_out =
        Zlib.deflate zlib_str buf !i !len o_buf 0 (Bytes.length o_buf) flush
      in
      i := !i + used_in;
      len := !len - used_in;
      o_len := !o_len + used_out;
      write_out oc
    done
  in

  let write (oc : Out.t) : unit =
    let output buf i len = write_zlib ~flush:Zlib.Z_NO_FLUSH oc buf i len in

    let bchar = Bytes.create 1 in
    let output_char c =
      Bytes.set bchar 0 c;
      output bchar 0 1
    in

    let flush () =
      flush_zlib oc ~flush:Zlib.Z_FINISH;
      assert (!o_len = 0);
      oc.flush ()
    in
    let close () =
      flush ();
      Zlib.deflate_end zlib_str;
      oc.close ()
    in
    (* new output channel that compresses on the fly *)
    let oc' = { Out.flush; close; output; output_char } in
    w.write oc';
    oc'.close ()
  in

  W.make ~write ()

let split_on_char ?(f = fun x -> x) c s : string list =
  let rec loop acc i =
    match String.index_from s i c with
    | exception Not_found ->
      let acc =
        if i = String.length s then
          acc
        else
          f (String.sub s i (String.length s - i)) :: acc
      in
      List.rev acc
    | j ->
      let acc = f (String.sub s i (j - i)) :: acc in
      loop acc (j + 1)
  in
  loop [] 0

let accept_deflate (req : _ S.Request.t) =
  match S.Request.get_header req "Accept-Encoding" with
  | Some s -> List.mem "deflate" @@ split_on_char ~f:String.trim ',' s
  | None -> false

let has_deflate s =
  try Scanf.sscanf s "deflate, %s" (fun _ -> true) with _ -> false

(* decompress [req]'s body if needed *)
let decompress_req_stream_ ~buf_size (req : BS.t S.Request.t) : _ S.Request.t =
  match S.Request.get_header ~f:String.trim req "Transfer-Encoding" with
  (* TODO
     | Some "gzip" ->
       let req' = S.Request.set_header req "Transfer-Encoding" "chunked" in
       Some (req', decode_gzip_stream_)
  *)
  | Some s when has_deflate s ->
    (match Scanf.sscanf s "deflate, %s" (fun s -> s) with
    | tr' ->
      let body' = S.Request.body req |> decode_deflate_stream_ ~buf_size in
      req
      |> S.Request.set_header "Transfer-Encoding" tr'
      |> S.Request.set_body body'
    | exception _ -> req)
  | _ -> req

let compress_resp_stream_ ~compress_above ~buf_size (req : _ S.Request.t)
    (resp : S.Response.t) : S.Response.t =
  (* headers for compressed stream *)
  let update_headers h =
    h
    |> S.Headers.remove "Content-Length"
    |> S.Headers.set "Content-Encoding" "deflate"
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
      |> S.Response.update_headers update_headers
      |> S.Response.set_body (`Writer body)
    | `Stream str ->
      Log.debug (fun k -> k "encode stream response with deflate");
      let w = BS.to_writer str in
      resp
      |> S.Response.update_headers update_headers
      |> S.Response.set_body (`Writer (encode_deflate_writer_ ~buf_size w))
    | `Writer w ->
      Log.debug (fun k -> k "encode writer response with deflate");
      resp
      |> S.Response.update_headers update_headers
      |> S.Response.set_body (`Writer (encode_deflate_writer_ ~buf_size w))
    | `String _ | `Void -> resp
  ) else
    resp

let middleware ?(compress_above = 16 * 1024) ?(buf_size = 16 * 1_024) () :
    S.Middleware.t =
  let buf_size = max buf_size 1_024 in
  fun h req ~resp ->
    let req = decompress_req_stream_ ~buf_size req in
    h req ~resp:(fun response ->
        resp @@ compress_resp_stream_ ~buf_size ~compress_above req response)

let setup ?compress_above ?buf_size server =
  let m = middleware ?compress_above ?buf_size () in
  Log.info (fun k -> k "setup gzip middleware");
  S.add_middleware ~stage:`Encoding server m
