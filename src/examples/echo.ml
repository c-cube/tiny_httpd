
module S = Tiny_httpd

(*
open Jemalloc

let show_crt_info () =
  let b = string_of_int in
  try
    let memory = get_memory_stats () in
    Printf.sprintf "MALLOC: size %s, used %s, heap %s, free %s" (b memory.mapped) (b memory.active) (b memory.allocated) (b (memory.mapped - memory.active))
  with exn ->
    Printf.sprintf "MALLOC:? (error %s)" (Printexc.to_string exn)

let setup () =
  Memory.show_crt_info := show_crt_info;
  Memory.malloc_release := release_free_memory;
  ()
   *)

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "-p", Arg.Set_int port_, " set port";
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";
(*   let module P = CCPool.Make(struct let max_size = 30 end) in *)
  let server = S.create ~port:!port_ ~max_connections:!j
(*       ~new_thread:P.run *)
      () in
  (* say hello *)
  S.add_path_handler ~meth:`GET server
    "/hello/%s@/" (fun name _req -> S.Response.make_string (Ok ("hello " ^name ^"!\n")));
  (* echo request *)
  S.add_path_handler server
    "/echo" (fun req -> S.Response.make_string (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)));
  S.add_path_handler ~meth:`POST server
    "/debug/%B" (fun b _req -> S._enable_debug b; S.Response.make_string (Ok "ok"));
  S.add_path_handler ~meth:`POST server
    "/compact/" (fun _req -> Gc.compact(); Jemalloc.release_free_memory(); S.Response.make_string (Ok "gc.compact: done"));
  S.add_path_handler ~meth:`POST server
    "/quit/" (fun _req -> S.stop server; S.Response.make_string (Ok "bye"));
  S.add_path_handler ~meth:`PUT server
    "/upload/%s" (fun path req ->
        S._debug (fun k->k "start upload %S\n%!" path);
        try
          let oc = open_out @@ "/tmp/" ^ path in
          output_string oc req.S.Request.body;
          flush oc;
          S.Response.make_string (Ok "uploaded file")
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s" (Printexc.to_string e)
      );
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
(*   ignore @@ Thread.create (fun () -> Statmemprof_inuit.start 1e-4 300 2) (); *)
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
