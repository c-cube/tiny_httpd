
module S = SimpleHTTPServer
module Pf = Printf

let contains_dot_dot s =
  try
    String.iteri
      (fun i c ->
        if c='.' && i+1 < String.length s && String.get s (i+1) = '.' then raise Exit)
      s;
    false
  with Exit -> true

let header_html = "Content-Type", "text/html"

let html_list_dir ~parent d : string =
  let entries = Sys.readdir d in
  let body = Buffer.create 256 in
  Printf.bprintf body "<ul>\n";
  begin match parent with
    | None -> ()
    | Some p -> 
      Printf.bprintf body "  <li> <a href=\"/%s\"> (parent directory) </a> </li>\n" p;
  end;
  Array.iter
    (fun f ->
       let full = Filename.concat d f in
       if not @@ contains_dot_dot f then (
         Printf.bprintf body "  <li> <a href=\"/%s\"> %s %s </a> </li>\n"
           full (if Sys.is_directory full then "[dir]" else "") f;
       )
    )
    entries;
  Printf.bprintf body "</ul>\n";
  Buffer.contents body

let same_path a b =
  Filename.dirname a = Filename.dirname b &&
  Filename.basename a = Filename.basename b

let serve ~addr ~port (dir:string) : _ result = 
  let server = S.create ~addr ~port () in
  S.add_path_handler server ~meth:`GET "/%s"
    (fun _req path () ->
       let f = Filename.concat dir path in
       if contains_dot_dot f then (
         S.Response.fail ~code:403 "Path is forbidden";
       ) else if not (Sys.file_exists f) then (
         S.Response.fail ~code:404 "file not found";
       ) else if Sys.is_directory f then (
         S._debug (fun k->k "list dir %S (topdir %S)"  f dir);
         let body =
           html_list_dir f
             ~parent:(if same_path f dir then None else Some dir)
         in
         S.Response.make ~headers:[header_html] (Ok body)
       ) else (
         try
           let ic = open_in path in
           S.Response.make_raw_chunked ~code:200 (input ic)
         with e ->
           S.Response.fail ~code:500 "error while reading file: %s" (Printexc.to_string e)
       ));
  S.run server


let main () =
  let addr_ = ref "127.0.0.1" in
  let port_ = ref 8080 in
  let dir_ = ref "." in
  Arg.parse (Arg.align [
      "--addr", Set_string addr_, " address to listen on";
      "-a", Set_string addr_, " alias to --listen";
      "--port", Set_int port_, " port to listen on";
      "-p", Set_int port_, " alias to --port";
      "--dir", Set_string dir_, " directory to serve (default: \".\")";
    ]) (fun _ -> raise (Arg.Bad "no positional arguments")) "http_of_dir [options]";
  match serve ~addr:!addr_ ~port:!port_ !dir_ with
  | Ok () -> ()
  | Error e ->
    raise e

let () = main ()
