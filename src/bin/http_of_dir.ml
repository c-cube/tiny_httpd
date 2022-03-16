module S = Tiny_httpd
module U = Tiny_httpd_util
module D = Tiny_httpd_dir
module Pf = Printf

let serve ~config (dir:string) addr port j : _ result =
  let server = S.create ~max_connections:j ~addr ~port () in
  Printf.printf "serve directory %s on http://%(%s%):%d\n%!"
    dir (if S.is_ipv6 server then "[%s]" else "%s") addr port;

  D.add_dir_path ~config ~dir ~prefix:"" server;
  S.run server

let parse_size s : int =
  try Scanf.sscanf s "%dM" (fun n -> n * 1_024 * 1_024)
  with _ ->
  try Scanf.sscanf s "%dk" (fun n -> n * 1_024)
  with _ ->
  try int_of_string s
  with _ -> raise (Arg.Bad "invalid size (expected <int>[kM]?)")

let main () =
  let config =
    D.config ~dir_behavior:Index_or_lists ()
  in
  let dir_ = ref "." in
  let addr = ref "127.0.0.1" in
  let port = ref 8080 in
  let j    = ref 32 in
  Arg.parse (Arg.align [
      "--addr", Set_string addr, " address to listen on";
      "-a", Set_string addr, " alias to --listen";
      "--port", Set_int port, " port to listen on";
      "-p", Set_int port, " alias to --port";
      "--dir", Set_string dir_, " directory to serve (default: \".\")";
      "--debug", Unit (fun () -> S._enable_debug true), " debug mode";
      "--upload", Unit (fun () -> config.upload <- true), " enable file uploading";
      "--no-upload", Unit (fun () -> config.upload <- false), " disable file uploading";
      "--download", Unit (fun () -> config.download <- true), " enable file downloading";
      "--no-download", Unit (fun () -> config.download <- false), " disable file downloading";
      "--max-upload", String (fun i -> config.max_upload_size <- parse_size i),
        " maximum size of files that can be uploaded";
      "--auto-index",
      Bool (fun b -> config.dir_behavior <-
               (if b then Index_or_lists else Lists)),
      " <bool> automatically redirect to index.html if present";
      "--delete", Unit (fun () -> config.delete <- true), " enable `delete` on files";
      "--no-delete", Unit (fun () -> config.delete <- false), " disable `delete` on files";
      "-j", Set_int j, " maximum number of simultaneous connections";
    ]) (fun s -> dir_ := s) "http_of_dir [options] [dir]";
  match serve ~config !dir_ !addr !port !j with
  | Ok () -> ()
  | Error e ->
    raise e

let () = main ()
