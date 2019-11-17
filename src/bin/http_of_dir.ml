
module S = Tiny_httpd
module Pf = Printf

type config = {
  mutable addr: string;
  mutable port: int;
  mutable upload: bool;
  mutable max_upload_size: int;
}

let default_config () : config = {
  addr="127.0.0.1";
  port=8080;
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

let serve ~config (dir:string) : _ result = 
  let server = S.create ~addr:config.addr ~port:config.port () in
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
            Error (403, "must know size before hand: max upload size is " ^
                        string_of_int config.max_upload_size)
        )
      (fun path req ->
         let fpath = Filename.concat dir path in
         let oc =
           try open_out fpath
           with e ->
             S.Response.fail_raise ~code:403 "cannot upload to %S: %s"
               path (Printexc.to_string e)
         in
         output_string oc req.S.Request.body;
         flush oc;
         close_out oc;
         S.Response.make (Ok "upload successful")
      )
  );
  S.add_path_handler server ~meth:`GET "/%s"
    (fun path _req ->
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
  let config = default_config () in
  let dir_ = ref "." in
  Arg.parse (Arg.align [
      "--addr", String (fun s -> config.addr <- s), " address to listen on";
      "-a", String (fun s -> config.addr <- s), " alias to --listen";
      "--port", Int (fun x -> config.port <- x), " port to listen on";
      "-p", Int (fun x -> config.port <- x), " alias to --port";
      "--dir", Set_string dir_, " directory to serve (default: \".\")";
      "--no-upload", Unit (fun () -> config.upload <- false), " disable file uploading";
      "--max-upload", Int (fun i -> config.max_upload_size <- 1024 * 1024 * i),
      "maximum size of files that can be uploaded, in MB";
      "--debug", Unit (fun () -> S._enable_debug true), " debug mode";
    ]) (fun _ -> raise (Arg.Bad "no positional arguments")) "http_of_dir [options]";
  match serve ~config !dir_ with
  | Ok () -> ()
  | Error e ->
    raise e

let () = main ()
