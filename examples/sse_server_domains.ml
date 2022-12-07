
(* serves some streams of events *)

module S = Tiny_httpd
module D = Tiny_httpd_domains

let port = ref 8080
let nbd = ref (Domain.recommended_domain_count ())

let () =
  Arg.parse (Arg.align [
      "-p", Arg.Set_int port, " port to listen on";
      "-n", Arg.Set_int nbd, " set number of domains";
      "--debug", Arg.Bool S._enable_debug, " toggle debug";
    ]) (fun _ -> ()) "sse_clock [opt*]";

  let pool = Domainslib.Task.setup_pool ~num_domains:!nbd () in
  let new_thread = D.new_thread pool in
  let server = S.create ~port:!port ~new_thread () in

  let extra_headers = [
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Methods", "POST, GET, OPTIONS";
  ] in

  (* tick/tock goes the clock *)
  S.add_route_server_sent_handler server S.Route.(exact "clock" @/ return)
    (fun _req (module EV : S.SERVER_SENT_GENERATOR) ->
       S._debug (fun k->k"new connection");
       EV.set_headers extra_headers;
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

  (* just count *)
  S.add_route_server_sent_handler server S.Route.(exact "count" @/ return)
    (fun _req (module EV : S.SERVER_SENT_GENERATOR)  ->
       let n = ref 0 in
       while true do
         EV.send_event ~data:(string_of_int !n) ();
         incr n;
         Unix.sleepf 0.1;
       done;
    );
  S.add_route_server_sent_handler server S.Route.(exact "count" @/ int @/ return)
    (fun n _req (module EV : S.SERVER_SENT_GENERATOR)  ->
       for i=0 to n do
         EV.send_event ~data:(string_of_int i) ();
         Unix.sleepf 0.1;
       done;
       EV.close();
    );

  Printf.printf "listening on http://localhost:%d/\n%!" (S.port server);
  match D.run pool server with
  | Ok () -> ()
  | Error e ->
    Printf.eprintf "error: %s\n%!" (Printexc.to_string e); exit 1