module S = Tiny_httpd
open Tiny_httpd_core

let pf = Printf.printf

let setup_logging ~debug () =
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level ~all:true
  @@ Some
       (if debug then
         Logs.Debug
       else
         Logs.Info)

let reply_buf (req : unit Request.t) ic oc =
  Log.info (fun k ->
      k "buf: new client connection from %s"
        (Util.show_sockaddr req.client_addr));

  let buf = Bytes.create 1024 in
  let continue = ref true in
  while !continue do
    match IO.Input.input ic buf 0 (Bytes.length buf) with
    | 0 -> continue := false
    | exception End_of_file -> continue := false
    | n ->
      pf "read %d bytes: \"%s\"\n%!" n (Bytes.sub_string buf 0 n);
      (* echo back *)
      IO.Output.output oc buf 0 n;
      IO.Output.flush oc
  done;
  Log.info (fun k -> k "client exiting")

let reply_by_line (req : unit Request.t) ic oc =
  Log.info (fun k ->
      k "by line: new client connection from %s"
        (Util.show_sockaddr req.client_addr));

  let continue = ref true in
  while !continue do
    match IO.Input.input_line ic with
    | exception End_of_file -> continue := false
    | None -> continue := false
    | Some line ->
      pf "read line: \"%s\"\n%!" line;
      (* echo back *)
      IO.Output.output_line oc line;
      IO.Output.flush oc
  done;
  Log.info (fun k -> k "client exiting")

let () =
  let port_ = ref 8080 in
  let j = ref 16 in
  let debug = ref false in
  Arg.parse
    (Arg.align
       [
         "--port", Arg.Set_int port_, " set port";
         "-p", Arg.Set_int port_, " set port";
         "--debug", Arg.Set debug, " enable debug";
         "-j", Arg.Set_int j, " maximum number of connections";
       ])
    (fun _ -> raise (Arg.Bad ""))
    "test-ws-server [option]*";
  setup_logging ~debug:!debug ();

  let server = S.create ~port:!port_ ~max_connections:!j () in
  Tiny_httpd_ws.add_route_handler server
    Route.(exact "line" @/ return)
    reply_by_line;
  Tiny_httpd_ws.add_route_handler server Route.(exact "buf" @/ return) reply_buf;

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
