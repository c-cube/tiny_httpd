module S = Tiny_httpd
open Tiny_httpd_core

let setup_logging ~debug () =
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level ~all:true
  @@ Some
       (if debug then
          Logs.Debug
        else
          Logs.Info)

let handle_ws (req : unit Request.t) ic oc =
  Log.info (fun k ->
      k "new client connection from %s" (Util.show_sockaddr req.client_addr));

  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        while true do
          Thread.delay 3.;
          IO.Output.output_string oc "(special ping!)";
          IO.Output.flush oc
        done)
      ()
  in

  let buf = Bytes.create 32 in
  let continue = ref true in
  while !continue do
    let n = IO.Input.input ic buf 0 (Bytes.length buf) in
    Log.debug (fun k ->
        k "echo %d bytes from websocket: %S" n (Bytes.sub_string buf 0 n));

    if n = 0 then continue := false;
    IO.Output.output oc buf 0 n;
    IO.Output.flush oc
  done;
  Log.info (fun k -> k "client exiting")

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
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
    "echo [option]*";
  setup_logging ~debug:!debug ();

  let server = S.create ~port:!port_ ~max_connections:!j () in
  Tiny_httpd_ws.add_route_handler server
    Route.(exact "echo" @/ return)
    handle_ws;

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
