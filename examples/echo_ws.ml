module S = Tiny_httpd
module Log = Tiny_httpd.Log
module IO = Tiny_httpd_io

let setup_logging ~debug () =
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level ~all:true
  @@ Some
       (if debug then
         Logs.Debug
       else
         Logs.Info)

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
    S.Route.(exact "echo" @/ return)
    (fun addr ic oc ->
      Log.info (fun k -> k "new client connection");
      let buf = Bytes.create 32 in
      let continue = ref true in
      while !continue do
        let n = IO.Input.input ic buf 0 (Bytes.length buf) in
        Log.debug (fun k ->
            k "echo %d bytes from websocket: %s" n (Bytes.sub_string buf 0 n));

        if n = 0 then continue := false;
        IO.Output.output oc buf 0 n;
        IO.Output.flush oc
      done;
      Log.info (fun k -> k "client exiting"));

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
