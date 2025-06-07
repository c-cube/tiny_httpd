(* serves some streams of events *)

open Tiny_httpd_core

let port = ref 8080

let () =
  Arg.parse
    (Arg.align
       [
         "-p", Arg.Set_int port, " port to listen on";
         "--debug", Arg.Unit (Log.setup ~debug:true), " enable debug";
       ])
    (fun _ -> ())
    "sse_clock [opt*]";
  let server = Tiny_httpd.create ~port:!port () in

  let extra_headers =
    [
      "Access-Control-Allow-Origin", "*";
      "Access-Control-Allow-Methods", "POST, GET, OPTIONS";
    ]
  in

  (* tick/tock goes the clock *)
  Server.add_route_server_sent_handler server
    Route.(exact "clock" @/ return)
    (fun _req (module EV : Server.SERVER_SENT_GENERATOR) ->
      Log.debug (fun k -> k "new SSE connection");
      EV.set_headers extra_headers;
      let tick = ref true in
      while true do
        let now = Ptime_clock.now () in
        Log.debug (fun k ->
            k "send clock ev %s" (Format.asprintf "%a" Ptime.pp now));
        EV.send_event
          ~event:
            (if !tick then
               "tick"
             else
               "tock")
          ~data:(Ptime.to_rfc3339 now) ();
        tick := not !tick;

        Unix.sleepf 1.0
      done);

  (* just count *)
  Server.add_route_server_sent_handler server
    Route.(exact "count" @/ return)
    (fun _req (module EV : Server.SERVER_SENT_GENERATOR) ->
      let n = ref 0 in
      while true do
        EV.send_event ~data:(string_of_int !n) ();
        incr n;
        Unix.sleepf 0.1
      done);
  Server.add_route_server_sent_handler server
    Route.(exact "count" @/ int @/ return)
    (fun n _req (module EV : Server.SERVER_SENT_GENERATOR) ->
      for i = 0 to n do
        EV.send_event ~data:(string_of_int i) ();
        Unix.sleepf 0.1
      done;
      EV.close ());

  Printf.printf "listening on http://localhost:%d/\n%!" (Server.port server);
  match Server.run server with
  | Ok () -> ()
  | Error e ->
    Printf.eprintf "error: %s\n%!" (Printexc.to_string e);
    exit 1
