
module S = Tiny_httpd
module BS = Tiny_httpd.Byte_stream

let mk_decode_deflate_stream_ ~buf_size () (is:S.byte_stream) : S.byte_stream =
  S._debug (fun k->k "wrap stream with deflate.decode");
  let buf = Bytes.make buf_size ' ' in
  let buf_len = ref 0 in
  let write_offset = ref 0 in
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
  in
  let bs_fill_buf () : _*_*_ =
    (* refill [buf] if needed *)
    if !write_offset >= !buf_len && not !is_done then (
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
    buf, !write_offset, !buf_len - !write_offset
  in
  {S.bs_fill_buf; bs_consume; bs_close}

let encode_deflate_stream_ ~buf_size (is:S.byte_stream) : S.byte_stream =
  S._debug (fun k->k "wrap stream with deflate.encode");
  let refill = ref true in
  let buf = Bytes.make buf_size ' ' in
  let buf_len = ref 0 in
  let write_offset = ref 0 in
  let zlib_str = Zlib.deflate_init 4 false in
  let bs_close () =
    S._debug (fun k->k "deflate: close");
    Zlib.deflate_end zlib_str;
    BS.close is
  in
  let bs_consume n =
    write_offset := n + !write_offset
  in
  let bs_fill_buf () =
    let rec loop() =
      S._debug (fun k->k "deflate.fill.iter out_off=%d out_len=%d"
                   !write_offset !buf_len);
      if !write_offset < !buf_len then (
        (* still the same slice, not consumed entirely by output *)
        buf, !write_offset, !buf_len - !write_offset
      ) else if not !refill then (
        (* empty slice, no refill *)
        buf, !write_offset, !buf_len - !write_offset
      ) else (
        (* the output was entirely consumed, we need to do more work *)
        write_offset := 0;
        buf_len := 0;
        let in_s, in_i, in_len = is.S.bs_fill_buf () in
        if in_len>0 then (
          (* try to decompress from input buffer *)
          let _finished, used_in, used_out =
            Zlib.deflate zlib_str
              in_s in_i in_len
              buf 0 (Bytes.length buf)
              Zlib.Z_NO_FLUSH
          in
          buf_len := used_out;
          is.S.bs_consume used_in;
          S._debug
            (fun k->k "encode %d bytes as %d bytes using deflate (finished: %b)"
                used_in used_out _finished);
          if _finished then (
            S._debug (fun k->k "deflate: finished");
            refill := false;
          );
          loop()
        ) else (
          (* finish sending the internal state *)
          let _finished, used_in, used_out =
            Zlib.deflate zlib_str
              in_s in_i in_len
              buf 0 (Bytes.length buf)
              Zlib.Z_FULL_FLUSH
          in
          assert (used_in = 0);
          buf_len := used_out;
          if used_out = 0 then (
            refill := false;
          );
          loop()
        )
      )
    in
    try loop()
    with Zlib.Error (e1,e2) ->
      S.Response.fail_raise ~code:400
        "deflate: error during compression:\n%s %s" e1 e2
  in
  {S.bs_fill_buf; bs_consume; bs_close}

let split_on_char ?(f=fun x->x) c s : string list =
  let rec loop acc i =
    match String.index_from s i c with
    | exception Not_found ->
      let acc =
        if i=String.length s then acc
        else f (String.sub s i (String.length s-i)) :: acc
      in List.rev acc
    | j ->
      let acc = f (String.sub s i (j-i)) :: acc in
      loop acc (j+1)
  in
  loop [] 0

let accept_deflate (req:_ S.Request.t) =
  match
    S.Request.get_header req "Accept-Encoding"
  with
  | Some s -> List.mem "deflate" @@ split_on_char ~f:String.trim ',' s
  | None -> false

let has_deflate s =
  try Scanf.sscanf s "deflate, %s" (fun _ -> true)
  with _ -> false

let cb_decode_compressed_stream ~buf_size (req:unit S.Request.t) : _ option =
  match S.Request.get_header ~f:String.trim req "Transfer-Encoding" with
  (* TODO
    | Some "gzip" ->
      let req' = S.Request.set_header req "Transfer-Encoding" "chunked" in
      Some (req', decode_gzip_stream_)
  *)
  | Some s when has_deflate s ->
    begin match Scanf.sscanf s "deflate, %s" (fun s -> s) with
      | tr' ->
        let req' = S.Request.set_header req "Transfer-Encoding" tr' in
        Some (req', mk_decode_deflate_stream_ ~buf_size ())
      | exception _ -> None
    end
  | _ -> None

let cb_encode_compressed_stream
    ~compress_above
    ~buf_size (req:_ S.Request.t) (resp:S.Response.t) : _ option =
  if accept_deflate req then (
    let set_headers h =
      h
      |> S.Headers.remove "Content-Length"
      |> S.Headers.set "Content-Encoding" "deflate"
      in
    match resp.body with
    | `String s when String.length s > compress_above ->
      S._debug
        (fun k->k "encode str response with deflate (size %d, threshold %d)"
             (String.length s) compress_above);
      let body =
        encode_deflate_stream_ ~buf_size @@ S.Byte_stream.of_string s
      in
      Some {
        resp with
        headers=set_headers resp.headers; body=`Stream body;
      }
    | `Stream str ->
      S._debug (fun k->k "encode stream response with deflate");
      Some {
        resp with
        headers= set_headers resp.headers;
        body=`Stream (encode_deflate_stream_ ~buf_size str);
      }
    | `String _ -> None
  ) else None

let setup
    ?(compress_above=500*1024)
    ?(buf_size=48 * 1_024) (server:S.t) : unit =
  let buf_size = max buf_size 1_024 in
  S._debug (fun k->k "setup gzip support (buf-size %d)" buf_size);
  S.add_decode_request_cb server (cb_decode_compressed_stream ~buf_size);
  S.add_encode_response_cb server (cb_encode_compressed_stream ~compress_above ~buf_size);
  ()

