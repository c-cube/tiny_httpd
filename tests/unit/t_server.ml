open Test_util
open Tiny_httpd_core

let () =
  let q =
    "GET hello HTTP/1.1\r\n\
     Host: coucou\r\n\
     Content-Length: 11\r\n\
     \r\n\
     salutationsSOMEJUNK"
  in
  let str = IO.Input_with_timeout.of_string q in
  let client_addr = Unix.(ADDR_INET (inet_addr_loopback, 1024)) in

  let deadline = Time.now_s () +. 10. in
  let r =
    Request.Private_.parse_req_start_exn ~client_addr ~buf:(Buf.create ())
      ~deadline str
  in
  match r with
  | None -> failwith "should parse"
  | Some req ->
    assert_eq (Some "coucou") (Headers.get "Host" req.headers);
    assert_eq (Some "coucou") (Headers.get "host" req.headers);
    assert_eq (Some "11") (Headers.get "content-length" req.headers);
    assert_eq "hello" req.path;
    let req =
      Request.Private_.parse_body req str |> Request.read_body_full ~deadline
    in
    assert_eq ~to_string:(fun s -> s) "salutations" req.body;
    ()
