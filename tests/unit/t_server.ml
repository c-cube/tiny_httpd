open Test_util
open Tiny_httpd_server

let () =
  let q =
    "GET hello HTTP/1.1\r\n\
     Host: coucou\r\n\
     Content-Length: 11\r\n\
     \r\n\
     salutationsSOMEJUNK"
  in
  let str = Tiny_httpd.Byte_stream.of_string q in
  let client_addr = Unix.(ADDR_INET (inet_addr_loopback, 1024)) in
  let r = Request.Internal_.parse_req_start ~client_addr ~get_time_s:(fun _ -> 0.) str in
  match r with
  | None -> failwith "should parse"
  | Some req ->
    assert_eq (Some "coucou") (Headers.get "Host" req.Request.headers);
    assert_eq (Some "coucou") (Headers.get "host" req.Request.headers);
    assert_eq (Some "11") (Headers.get "content-length" req.Request.headers);
    assert_eq "hello" req.Request.path;
    let req = Request.Internal_.parse_body req str |> Request.read_body_full in
    assert_eq ~to_string:(fun s -> s) "salutations" req.Request.body;
    ()
