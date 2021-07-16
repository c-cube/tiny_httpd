let addr = ref "127.0.0.1"
let port = ref 8080

let bufsize = 1024

let () =
  Arg.parse (Arg.align [
      "-p", Arg.Set_int port, " port to connect to";
    ]) (fun s -> addr := s) "sse_client [opt]* [addr]?";

  Format.printf "connect to %s:%d@." !addr !port;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string !addr, !port));
  Unix.setsockopt sock Unix.TCP_NODELAY false;

  let ic = Unix.in_channel_of_descr sock in
  let oc = Unix.out_channel_of_descr sock in
  output_string oc "GET /clock HTTP/1.1\r\nHost: localhost\r\n\r\n";
  flush oc;

  let continue = ref true in
  let buf = Bytes.create bufsize in
  while !continue do
    let n = input ic buf 0 bufsize in
    if n=0 then continue := false;
    output stdout buf 0 n; flush stdout
  done;
  Format.printf "bye!@."
