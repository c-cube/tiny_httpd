
module S = SimpleHTTPServer

let () =
  let server = S.create () in
  (* say hello *)
  S.add_path_handler ~meth:`GET server
    "/hello/%s@/" (fun _req name () -> S.Response.make_ok ("hello " ^name ^"!\n"));
  (* echo request *)
  S.add_path_handler server
    "/echo" (fun req () -> S.Response.make_ok (Format.asprintf "echo:@ %a@." S.Request.pp req));
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
