open Test_util
open Tiny_httpd_core.IO

let spf = Printf.sprintf

(* one chunk *)
let () =
  let io = Input.of_string "5\r\nhello\r\n0\r\n\r\nARGH" in
  let str =
    io
    |> Input.read_chunked ~bytes:(Bytes.create 4) ~fail:failwith
    |> Input.read_all_using ~buf:(Buf.create ())
  in
  assert_eq ~to_string:(spf "%S") "hello" str

(* two chunks *)
let () =
  let io = Input.of_string "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\nARGH" in
  let str =
    io
    |> Input.read_chunked ~bytes:(Bytes.create 4) ~fail:failwith
    |> Input.read_all_using ~buf:(Buf.create ())
  in
  assert_eq ~to_string:(spf "%S") "hello world" str;

  let str_rest = io |> Input.read_all_using ~buf:(Buf.create ()) in
  assert_eq ~to_string:(spf "%S") "ARGH" str_rest;
  ()

(* two chunks *)
let () =
  let io = Input.of_string "10\r\n{\"poCheck\":true}\r\n0\r\n\r\n" in
  let str =
    io
    |> Input.read_chunked ~bytes:(Bytes.create 4) ~fail:failwith
    |> Input.read_all_using ~buf:(Buf.create ())
  in
  assert_eq ~to_string:(spf "%S") {|{"poCheck":true}|} str;

  let str_rest = io |> Input.read_all_using ~buf:(Buf.create ()) in
  assert_eq ~to_string:(spf "%S") "" str_rest;
  ()
