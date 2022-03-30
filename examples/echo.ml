
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

(* ugly AF *)
let base64 x =
  let ic, oc = Unix.open_process "base64" in
  output_string oc x; flush oc; close_out oc;
  let r = input_line ic in
  ignore (Unix.close_process (ic,oc));
  r

let () =
  let port_ = ref 8080 in
  let pool = ref false in
  let pool_size = ref 0 in
  let j = ref 32 in
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "-p", Arg.Set_int port_, " set port";
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "--pool", Arg.Set pool, " use a thread pool";
      "--pool-size", Arg.Set_int pool_size, " size of thread pool if --pool is enabled";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let new_thread =
    if !pool then (
      let j =
        if !pool_size > 0 then Some !pool_size
        else if !j > 64 then Some (!j/4) else None in
      let pool = Tiny_httpd_pool.create ?j () in
      Some (fun f -> Tiny_httpd_pool.run pool f)
    ) else None
  in
  let server = S.create ?new_thread ~port:!port_ ~max_connections:!j () in
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
        let str =
          S.alloc_buf_for_stream server @@ fun buf ->
          S.Byte_stream.of_chan ~buf ic in
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

  (* protected by login *)
  S.add_route_handler server S.Route.(exact "protected" @/ return)
    (fun req ->
       let ok =
         match S.Request.get_header req "authorization" with
         | Some v ->
           S._debug(fun k->k"authenticate with %S" v);
           v = "Basic " ^ base64 "user:foobar"
         | None -> false
       in
       if ok then (
         (* FIXME: a logout link *)
         let s = "<p>hello, this is super secret!</p><a href=\"/logout\">log out</a>" in
         S.Response.make_string (Ok s)
       ) else (
         let headers = S.Headers.(empty |> set "www-authenticate" "basic realm=\"echo\"") in
         S.Response.fail ~code:401 ~headers "invalid"
       ));

  (* logout *)
  S.add_route_handler server S.Route.(exact "logout" @/ return)
    (fun _req -> S.Response.fail ~code:401 "logged out");

  (* stats *)
  S.add_route_handler server S.Route.(exact "stats" @/ return)
    (fun _req ->
       let stats = get_stats() in
       S.Response.make_string @@ Ok stats
    );

  (* VFS *)
  Tiny_httpd_dir.add_vfs server
    ~config:(Tiny_httpd_dir.config ~download:true
               ~dir_behavior:Tiny_httpd_dir.Index_or_lists ())
    ~vfs:Vfs.vfs ~prefix:"vfs";

  (* quit *)
  S.add_route_handler server ~meth:`POST S.Route.(exact "quit" @/ return)
    (fun _ -> S.stop server; S.Response.make_string @@ Ok "stopping…");

  (* main page *)
  S.add_route_handler server S.Route.(return)
    (fun _req ->
       let open Tiny_httpd_html in
       let h = html [] [
           head[][title[][txt "index of echo"]];
           body[][
             h3[] [txt "welcome!"];
             p[] [b[] [txt "endpoints are:"]];
             ul[] [
               li[][pre[][txt "/hello/:name (GET)"]];
               li[][pre[][a[A.href "/echo/"][txt "echo"]; txt " echo back query"]];
               li[][pre[][txt "/upload/:path (PUT) to upload a file"]];
               li[][pre[][txt "/zcat/:path (GET) to download a file (deflate transfer-encoding)"]];
               li[][pre[][a[A.href "/stats/"][txt"/stats/"]; txt" (GET) to access statistics"]];
               li[][pre[][a[A.href "/vfs/"][txt"/vfs"]; txt" (GET) to access a VFS embedded in the binary"]];
               li[][
                 pre[A.style "display: inline"][
                   a[A.href "/quit"][txt "/quit"]; txt" (POST) to stop server"
                 ];
                 form[A.style "display: inline";
                      A.action "/quit"; A.method_ "POST";
                     ][
                   button
                     [A.type_ "submit"]
                     [txt"quit"]
                 ];
               ];
               li[][pre[][a[A.href "/protected"][txt "/protected"]; txt" (GET) to see a protected page (login: user, password: foobar)"]];
               li[][pre[][a[A.href "/logout"][txt "/logout"]; txt" (POST) to log out"]];
             ]
           ]
         ] in
       let s = to_string_top h in
       S.Response.make_string ~headers:["content-type", "text/html"] @@ Ok s);

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
