
module S = Tiny_httpd

let debug_ k =
  if None<>Sys.getenv_opt "HTTP_DBG" then (
    k (fun fmt -> Printf.kfprintf (fun oc -> k (Printf.fprintf oc)) stdout fmt)
  )

let () =
  let j = ref 32 in
  Arg.parse (Arg.align [
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";
  let server = S.create ~max_connections:!j () in
  (* say hello *)
  S.add_path_handler ~meth:`GET server
    "/hello/%s@/" (fun name _req -> S.Response.make_string (Ok ("hello " ^name ^"!\n")));
  (* echo request *)
  S.add_path_handler server
    "/echo" (fun req -> S.Response.make_string (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)));
  S.add_path_handler ~meth:`PUT server
    "/upload/%s" (fun path req ->
        debug_ (fun k->k "start upload %S\n%!" path);
        try
          let oc = open_out @@ "/tmp/" ^ path in
          output_string oc req.S.Request.body;
          flush oc;
          S.Response.make_string (Ok "uploaded file")
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s" (Printexc.to_string e)
      );
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
