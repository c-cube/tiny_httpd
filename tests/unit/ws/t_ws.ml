open Test_util

let read_file file : string =
  let buf = Buffer.create 32 in
  let ic = open_in_bin file in
  Buffer.add_channel buf ic (in_channel_length ic);
  Buffer.contents buf

let apply_masking = Tiny_httpd_ws.Private_.apply_masking

let decode ~key b =
  let buf = Bytes.copy b in
  apply_masking ~mask_key:key buf 0 (Bytes.length buf);
  buf

let () =
  let key = "\x57\x7d\xfd\x95" |> Bytes.unsafe_of_string in
  let content = read_file "masked.data" in
  let decoded = decode ~key (Bytes.unsafe_of_string content) in
  print_endline (Bytes.unsafe_to_string decoded);
  ()
