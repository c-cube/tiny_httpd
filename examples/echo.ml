
module S = Tiny_httpd

let now_ = Unix.gettimeofday

(* util: a little middleware collecting statistics *)
let middleware_stat () : S.Middleware.t * (unit -> string) =
  let n_req = ref 0 in
  let total_time_ = ref 0. in
  let parse_time_ = ref 0. in
  let build_time_ = ref 0. in
  let write_time_ = ref 0. in

  let m h req ~resp =
    incr n_req;
    let t1 = S.Request.start_time req in
    let t2 = now_ () in
    h req ~resp:(fun response ->
        let t3 = now_ () in
        resp response;
        let t4 = now_ () in
        total_time_ := !total_time_ +. (t4 -. t1);
        parse_time_ := !parse_time_ +. (t2 -. t1);
        build_time_ := !build_time_ +. (t3 -. t2);
        write_time_ := !write_time_ +. (t4 -. t3);
      )
  and get_stat () =
    Printf.sprintf "%d requests (average response time: %.3fms = %.3fms + %.3fms + %.3fms)"
      !n_req (!total_time_ /. float !n_req *. 1e3)
             (!parse_time_ /. float !n_req *. 1e3)
             (!build_time_ /. float !n_req *. 1e3)
             (!write_time_ /. float !n_req *. 1e3)
  in
  m, get_stat


let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "-p", Arg.Set_int port_, " set port";
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let server = S.create ~port:!port_ ~max_connections:!j () in
  Tiny_httpd_camlzip.setup ~compress_above:1024 ~buf_size:(16*1024) server;

  let m_stats, get_stats = middleware_stat () in
  S.add_middleware server ~stage:(`Stage 1) m_stats;

  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "hello" @/ string @/ return)
    (fun name _req -> S.Response.make_string (Ok ("hello " ^name ^"!\n")));

  (* compressed file access *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "zcat" @/ string_urlencoded @/ return)
    (fun path _req ->
        let ic = open_in path in
        let str = S.Byte_stream.of_chan ic in
        let mime_type =
          try
            let p = Unix.open_process_in (Printf.sprintf "file -i -b %S" path) in
            try
              let s = ["Content-Type", String.trim (input_line p)] in
              ignore @@ Unix.close_process_in p;
              s
            with _ -> ignore @@ Unix.close_process_in p; []
          with _ -> []
        in
        S.Response.make_stream ~headers:mime_type (Ok str)
      );

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        let q =
          S.Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
          |> String.concat ";"
        in
        S.Response.make_string
          (Ok (Format.asprintf "echo:@ %a@ (query: %s)@." S.Request.pp req q)));

  (* file upload *)
  S.add_route_handler_stream ~meth:`PUT server
    S.Route.(exact "upload" @/ string @/ return)
    (fun path req ->
        S._debug (fun k->k "start upload %S, headers:\n%s\n\n%!" path
                     (Format.asprintf "%a" S.Headers.pp (S.Request.headers req)));
        try
          let oc = open_out @@ "/tmp/" ^ path in
          S.Byte_stream.to_chan oc req.S.Request.body;
          flush oc;
          S.Response.make_string (Ok "uploaded file")
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s" (Printexc.to_string e)
      );

  (* stats *)
  S.add_route_handler server S.Route.(exact "stats" @/ return)
    (fun _req ->
       let stats = get_stats() in
       S.Response.make_string @@ Ok stats
    );

  (* main page *)
  S.add_route_handler server S.Route.(return)
    (fun _req ->
       let s = "<head></head><body>\n\
                <p><b>welcome!</b>\n<p>endpoints are:\n<ul>\
                <li><pre>/hello/'name' (GET)</pre></li>\n\
                <li><pre>/echo/ (GET) echoes back query</pre></li>\n\
                <li><pre>/upload/'path' (PUT) to upload a file</pre></li>\n\
                <li><pre>/zcat/'path' (GET) to download a file (compressed)</pre></li>\n\
                <li><pre>/stats/ (GET) to access statistics</pre></li>\n\
                </ul></body>"
       in
       S.Response.make_string ~headers:["content-type", "text/html"] @@ Ok s);

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
