
(* serves a stream of clock events *)

module S = Tiny_httpd

let port = ref 8080

let () =
  Arg.parse (Arg.align [
      "-p", Arg.Set_int port, " port to listen on";
      "--debug", Arg.Bool S._enable_debug, " toggle debug";
    ]) (fun _ -> ()) "sse_clock [opt*]";
  let server = S.create ~port:!port () in

  S.add_route_server_sent_handler server S.Route.(exact "clock" @/ return)
    (fun _req (module EV : S.SERVER_SENT_GENERATOR) ->
       S._debug (fun k->k"new connection");
       let tick = ref true in
       while true do
         let now = Ptime_clock.now() in
         S._debug (fun k->k"send clock ev %s" (Format.asprintf "%a" Ptime.pp now));
         EV.send_event ~event:(if !tick then "tick" else "tock")
           ~data:(Ptime.to_rfc3339 now) ();
         tick := not !tick;

         Unix.sleepf 1.0;
       done;
    );

  Printf.printf "listening on http://localhost:%d/\n%!" (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e ->
    Printf.eprintf "error: %s\n%!" (Printexc.to_string e); exit 1


