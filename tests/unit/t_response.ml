open Test_util
open Tiny_httpd_core
module U = Util

let () =
  let res =
    Response.make_raw ~code:200 ~headers:[ "content-length", "42" ] ""
  in
  let h = Headers.get_exn "content-length" res.headers in
  assert_eq "42" h

let () =
  let res =
    Response.make_raw ~code:200 ~headers:[ "Content-Length", "42" ] ""
  in
  let h = Headers.get_exn "content-length" res.headers in
  assert_eq "42" h
