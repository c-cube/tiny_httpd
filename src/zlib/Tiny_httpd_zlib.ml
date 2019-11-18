
module S = Tiny_httpd
module BS = Tiny_httpd.Byte_stream

let mk_decode_deflate_stream_ () (is:S.byte_stream) : S.byte_stream =
  S._debug (fun k->k "wrap stream with inflate.decode");
  let buf = Bytes.make 4096 ' ' in
  let write_offset = ref 0 in
  let buf_len = ref 0 in
  let zlib_str = Zlib.inflate_init false in
  let is_done = ref false in
  let bs_close () =
    Zlib.inflate_end zlib_str;
    BS.close is
  in
  let bs_consume len : unit =
    if len > !buf_len then (
      S.Response.fail_raise ~code:400
        "inflate: error during decompression: invalid consume len %d (max %d)"
        len !buf_len
    );
    write_offset := !write_offset + len;
    buf_len := !buf_len - len;
  in
  let bs_fill_buf () : _*_*_ =
    (* refill [buf] if needed *)
    if !buf_len = 0 && not !is_done then (
      let ib, ioff, ilen = is.S.bs_fill_buf () in
      begin
        try
          let finished, used_in, used_out =
            Zlib.inflate zlib_str
              buf 0 (Bytes.length buf)
              ib ioff ilen Zlib.Z_SYNC_FLUSH
          in
          is.S.bs_consume used_in;
          write_offset := 0;
          buf_len := used_out;
          if finished then is_done := true;
          S._debug (fun k->k "decode %d bytes as %d bytes from inflate (finished: %b)"
                       used_in used_out finished);
        with Zlib.Error (e1,e2) ->
          S.Response.fail_raise ~code:400
            "inflate: error during decompression:\n%s %s" e1 e2
      end;
      S._debug (fun k->k "inflate: refill %d bytes into internal buf" !buf_len);
    );
    buf, !write_offset, !buf_len
  in
  {S.bs_fill_buf; bs_consume; bs_close}

let has_deflate s =
  try Scanf.sscanf s "deflate; %s" (fun _ -> true)
  with _ -> false

let cb_decode_compressed_stream (req:unit S.Request.t) : _ option =
  match S.Request.get_header ~f:String.trim req "Transfer-Encoding" with
  (* TODO
    | Some "gzip" ->
      let req' = S.Request.set_header req "Transfer-Encoding" "chunked" in
      Some (req', decode_gzip_stream_)
  *)
  | Some "deflate" ->
    let req' = S.Request.set_header req "Transfer-Encoding" "chunked" in
    Some (req', mk_decode_deflate_stream_ ())
  | Some s when has_deflate s ->
    begin match Scanf.sscanf s "deflate; %s" (fun s -> s) with
      | tr' ->
        let req' = S.Request.set_header req "Transfer-Encoding" tr' in
        Some (req', mk_decode_deflate_stream_ ())
      | exception _ -> None
    end
  | _ -> None

let setup (server:S.t) : unit =
  S._debug (fun k->k "setup gzip support");
  S.add_decode_request_cb server cb_decode_compressed_stream;
  (*   S.add_encode_response_cb server (assert false); *)
  ()

