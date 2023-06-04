open Test_util
open Tiny_httpd_buf

let spf = Printf.sprintf

let () =
  let b = create ~size:4 () in
  add_string b "hello";
  assert_eq ~to_string:(spf "%S") "hello" (contents b);

  add_string b " world";
  assert_eq ~to_string:(spf "%S") "hello world" (contents b);
  ()

let buffer_of_string str =
  let buf = Buffer.create 32 in
  Buffer.add_string buf str;
  buf

let () =
  let b = create ~size:4 () in
  add_buffer b (buffer_of_string "hello");
  assert_eq ~to_string:(spf "%S") "hello" (contents b);

  add_buffer b (buffer_of_string " world");
  assert_eq ~to_string:(spf "%S") "hello world" (contents b);
  ()
