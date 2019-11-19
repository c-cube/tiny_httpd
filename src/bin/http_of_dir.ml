
module S = Tiny_httpd
module Pf = Printf

type config = {
  mutable addr: string;
  mutable port: int;
  mutable upload: bool;
  mutable max_upload_size: int;
  mutable delete: bool;
}

let default_config () : config = {
  addr="127.0.0.1";
  port=8080;
  delete=false;
  upload=true;
  max_upload_size = 10 * 1024 * 1024;
}

let contains_dot_dot s =
  try
    String.iteri
      (fun i c ->
        if c='.' && i+1 < String.length s && String.get s (i+1) = '.' then raise Exit)
      s;
    false
  with Exit -> true

let header_html = "Content-Type", "text/html"
let (//) = Filename.concat

let html_list_dir ~top ~parent d : string =
  let entries = Sys.readdir @@ (top // d) in
  let body = Buffer.create 256 in
  Printf.bprintf body {|<head><title> http_of_dir %S</title>
  </head><body>
    <h2> Index of %S</h2>
  |} top d;
  begin match parent with
    | None -> ()
    | Some p -> 
      Printf.bprintf body "<a href=\"/%s\"> (parent directory) </a>\n" p;
  end;
  Printf.bprintf body "<ul>\n";
  Array.iter
    (fun f ->
       if not @@ contains_dot_dot (d // f) then (
         Printf.bprintf body "  <li> <a href=\"/%s\"> %s %s </a> </li>\n"
           (d // f) f (if Sys.is_directory (top // d // f) then "[dir]" else "");
       )
    )
    entries;
  Printf.bprintf body "</ul></body>\n";
  Buffer.contents body

let serve ~config (dir:string) : _ result = 
  Printf.printf "serve directory %s on http://%s:%d\n%!" dir config.addr config.port;
  let server = S.create ~addr:config.addr ~port:config.port () in
  if config.delete then (
    S.add_path_handler server ~meth:`DELETE "/%s"
      (fun path _req ->
         if contains_dot_dot path then (
           S.Response.fail_raise ~code:403 "invalid path in delete"
         );
         S.Response.make_string
           (try
              Sys.remove (dir // path); Ok "file deleted successfully"
            with e -> Error (500, Printexc.to_string e))
      );
  ) else (
    S.add_path_handler server ~meth:`DELETE "/%s"
      (fun _ _  -> S.Response.make_raw ~code:405 "delete not allowed");
  );
  if config.upload then (
    S.add_path_handler server ~meth:`PUT "/%s"
      ~accept:(fun req ->
          match S.Request.get_header_int req "Content-Length" with
          | Some n when n > config.max_upload_size ->
            Error (403, "max upload size is " ^ string_of_int config.max_upload_size)
          | Some _ when contains_dot_dot req.S.Request.path ->
            Error (403, "invalid path (contains '..')")
          | Some _ -> Ok ()
          | None ->
            Error (411, "must know size before hand: max upload size is " ^
                        string_of_int config.max_upload_size)
        )
      (fun path req ->
         let fpath = dir // path in
         let oc =
           try open_out fpath
           with e ->
             S.Response.fail_raise ~code:403 "cannot upload to %S: %s"
               path (Printexc.to_string e)
         in
         output_string oc req.S.Request.body;
         flush oc;
         close_out oc;
         S.Response.make_raw ~code:201 "upload successful"
      )
  ) else (
    S.add_path_handler server ~meth:`PUT "/%s"
      (fun _ _  -> S.Response.make_raw ~code:405 "upload not allowed");
  );
  S.add_path_handler server ~meth:`GET "/%s"
    (fun path _req ->
       let full_path = dir // path in
       if contains_dot_dot full_path then (
         S.Response.fail ~code:403 "Path is forbidden";
       ) else if not (Sys.file_exists full_path) then (
         S.Response.fail ~code:404 "File not found";
       ) else if Sys.is_directory full_path then (
         S._debug (fun k->k "list dir %S (topdir %S)"  full_path dir);
         let parent = Filename.(dirname path) in
         let parent = if parent <> path then Some parent else None in
         let body = html_list_dir ~top:dir path ~parent in
         S.Response.make_string ~headers:[header_html] (Ok body)
       ) else (
         try
           let ic = open_in full_path in
           S.Response.make_raw_stream ~code:200 (S.Stream_.of_chan ic)
         with e ->
           S.Response.fail ~code:500 "error while reading file: %s" (Printexc.to_string e)
       ));
  S.run server

let parse_size s : int =
  try Scanf.sscanf s "%dM" (fun n -> n * 1_024 * 1_024)
  with _ ->
  try Scanf.sscanf s "%dk" (fun n -> n * 1_024)
  with _ ->
  try int_of_string s
  with _ -> raise (Arg.Bad "invalid size (expected <int>[kM]?)")

let main () =
  let config = default_config () in
  let dir_ = ref "." in
  Arg.parse (Arg.align [
      "--addr", String (fun s -> config.addr <- s), " address to listen on";
      "-a", String (fun s -> config.addr <- s), " alias to --listen";
      "--port", Int (fun x -> config.port <- x), " port to listen on";
      "-p", Int (fun x -> config.port <- x), " alias to --port";
      "--dir", Set_string dir_, " directory to serve (default: \".\")";
      "--no-upload", Unit (fun () -> config.upload <- false), " disable file uploading";
      "--max-upload", String (fun i -> config.max_upload_size <- parse_size i),
      "maximum size of files that can be uploaded";
      "--debug", Unit (fun () -> S._enable_debug true), " debug mode";
      "--delete", Unit (fun () -> config.delete <- true), " enable `delete` on files";
      "--no-delete", Unit (fun () -> config.delete <- false), " disable `delete` on files";
    ]) (fun s -> dir_ := s) "http_of_dir [options] [dir]";
  match serve ~config !dir_ with
  | Ok () -> ()
  | Error e ->
    raise e

let () = main ()
