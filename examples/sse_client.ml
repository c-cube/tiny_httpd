let addr = ref "127.0.0.1"
let port = ref 8080
let path = ref "/clock"

let bufsize = 1024

let () =
  Arg.parse (Arg.align [
      "-h", Arg.Set_string addr, " address to connect to";
      "-p", Arg.Set_int port, " port to connect to";
      "--alarm", Arg.Int (fun i->Unix.alarm i|>ignore), " set alarm (in seconds)";
    ]) (fun s -> path := s) "sse_client [opt]* path?";

  Format.printf "connect to %s:%d@." !addr !port;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string !addr, !port));
  Unix.setsockopt sock Unix.TCP_NODELAY false;

  let ic = Unix.in_channel_of_descr sock in
  let oc = Unix.out_channel_of_descr sock in
  Printf.fprintf oc "GET %s HTTP/1.1\r\nHost: localhost\r\n\r\n" !path;
  flush oc;

  let continue = ref true in
  let buf = Bytes.create bufsize in
  while !continue do
    let n = input ic buf 0 bufsize in
    if n=0 then continue := false;
    output stdout buf 0 n; flush stdout
  done;
  Format.printf "exit!@."
