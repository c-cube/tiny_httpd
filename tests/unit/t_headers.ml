open Tiny_httpd_core

(* Test that header size limits are enforced *)
let test_header_too_large () =
  (* Create a header that's larger than 16KB *)
  let large_value = String.make 20000 'x' in
  let q =
    "GET / HTTP/1.1\r\nHost: example.com\r\nX-Large: " ^ large_value
    ^ "\r\n\r\n"
  in
  let str = IO.Input.of_string q in
  let client_addr = Unix.(ADDR_INET (inet_addr_loopback, 1024)) in
  let buf = Buf.create () in
  try
    let _ =
      Request.Private_.parse_req_start_exn ~client_addr ~buf
        ~get_time_s:(fun _ -> 0.)
        str
    in
    failwith "should have failed with 431"
  with Tiny_httpd_core.Response.Bad_req (431, _) -> () (* expected *)

let () = test_header_too_large ()
