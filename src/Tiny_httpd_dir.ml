module S = Tiny_httpd
module U = Tiny_httpd_util
module Pf = Printf

type dir_behavior =
  | Index | Lists | Index_or_lists | Forbidden

type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
}

let default_config () : config =
  { download=true;
    dir_behavior=Forbidden;
    delete=false;
    upload=false;
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

(* Human readable size *)
let human_size (x:int) : string =
  if x >= 1_000_000_000 then Printf.sprintf "%d.%dG" (x / 1_000_000_000) ((x/1_000_000) mod 1_000_000)
  else if x >= 1_000_000 then Printf.sprintf "%d.%dM" (x / 1_000_000) ((x/1000) mod 1_000)
  else if x >= 1_000 then Printf.sprintf "%d.%dk" (x/1000) ((x/100) mod 100)
  else Printf.sprintf "%db" x

let header_html = "Content-Type", "text/html"
let (//) = Filename.concat

let encode_path s = U.percent_encode ~skip:(function '/' -> true|_->false) s
let _decode_path s = match U.percent_decode s with Some s->s | None -> s

let is_hidden s = String.length s>0 && s.[0] = '.'

let html_list_dir ~top ~parent d : string =
  let entries = Sys.readdir @@ (top // d) in
  Array.sort compare entries;
  let body = Buffer.create 256 in
  (* TODO: breadcrumbs for the path, each element a link to the given ancestor dir *)
  Printf.bprintf body {|<head><title> http_of_dir %S</title><meta charset="utf-8">
  </head><body>
    <h2> Index of %S</h2>
  |} top d;
  begin match parent with
    | None -> ()
    | Some p ->
      Printf.bprintf body "<a href=\"/%s\"> (parent directory) </a>\n"
        (encode_path p);
  end;
  Printf.bprintf body "<ul>\n";
  let hidden_stop = ref 0 in
  Array.iteri
    (fun i f ->
       if is_hidden f && (i=0 || not (is_hidden entries.(i-1))) then (
         hidden_stop := i;
         while !hidden_stop < Array.length entries && is_hidden entries.(!hidden_stop) do
           incr hidden_stop;
         done;
         Printf.bprintf body "<details> <summary>(%d hidden files)</summary>\n" (!hidden_stop-i);
       ) else if i = !hidden_stop then (
         Printf.bprintf body "</details/>\n";
       );
       if not @@ contains_dot_dot (d // f) then (
         let fpath = top // d // f in
         if not @@ Sys.file_exists fpath then (
           Printf.bprintf body "  <li> %s [invalid file]</li>\n" f
         ) else (
           let size =
             try Printf.sprintf " (%s)" @@ human_size (Unix.stat fpath).Unix.st_size
             with _ -> ""
           in
           Printf.bprintf body "  <li> <a href=\"/%s\"> %s </a> %s%s </li>\n"
             (encode_path (d // f)) f (if Sys.is_directory fpath then "[dir]" else "") size
         );
       )
    )
    entries;
  Printf.bprintf body "</ul></body>\n";
  Buffer.contents body

let finally_ ~h x f =
  try
    let y = f x in
    h x;
    y
  with e ->
    h x;
    raise e

let add_dir_path ~config ~dir ~prefix server =

  if config.delete then (
    S.add_route_handler server ~meth:`DELETE
      S.Route.(exact_path prefix (rest_of_path_urlencoded))
      (fun path _req ->
         if contains_dot_dot path then (
           S.Response.fail_raise ~code:403 "invalid path in delete"
         ) else (
           S.Response.make_string
             (try
                Sys.remove (dir // path); Ok "file deleted successfully"
              with e -> Error (500, Printexc.to_string e))
         )
      );
  ) else (
    S.add_route_handler server ~meth:`DELETE
      S.Route.(exact_path prefix (S.Route.(string @/ return)))
      (fun _ _  -> S.Response.make_raw ~code:405 "delete not allowed");
  );

  if config.upload then (
    S.add_route_handler_stream server ~meth:`PUT
      S.Route.(exact_path prefix (rest_of_path_urlencoded))
      ~accept:(fun req ->
          match S.Request.get_header_int req "Content-Length" with
          | Some n when n > config.max_upload_size ->
            Error (403, "max upload size is " ^ string_of_int config.max_upload_size)
          | Some _ when contains_dot_dot req.S.Request.path ->
            Error (403, "invalid path (contains '..')")
          | _ -> Ok ()
        )
      (fun path req ->
         let fpath = dir // path in
         let oc =
           try open_out fpath
           with e ->
             S.Response.fail_raise ~code:403 "cannot upload to %S: %s"
               path (Printexc.to_string e)
         in
         let req = S.Request.limit_body_size ~max_size:config.max_upload_size req in
         S.Byte_stream.to_chan oc req.S.Request.body;
         flush oc;
         close_out oc;
         S._debug (fun k->k "done uploading");
         S.Response.make_raw ~code:201 "upload successful"
      )
  ) else (
    S.add_route_handler server ~meth:`PUT
      S.Route.(exact_path prefix (string @/ return))
      (fun _ _  -> S.Response.make_raw ~code:405 "upload not allowed");
  );

  if config.download then (
    S.add_route_handler server ~meth:`GET
      S.Route.(exact_path prefix (rest_of_path_urlencoded))
      (fun path req ->
         let full_path = dir // path in
         let mtime = lazy (
           try Printf.sprintf "mtime: %f" (Unix.stat full_path).Unix.st_mtime
           with _ -> S.Response.fail_raise ~code:403 "Cannot access file"
         ) in
         if contains_dot_dot full_path then (
           S.Response.fail ~code:403 "Path is forbidden";
         ) else if not (Sys.file_exists full_path) then (
           S.Response.fail ~code:404 "File not found";
         ) else if S.Request.get_header req "If-None-Match" = Some (Lazy.force mtime) then (
           S._debug (fun k->k "cached object %S (etag: %S)" path (Lazy.force mtime));
           S.Response.make_raw ~code:304 ""
         ) else if Sys.is_directory full_path then (
           S._debug (fun k->k "list dir %S (topdir %S)"  full_path dir);
           let parent = Filename.(dirname path) in
           let parent = if parent <> path then Some parent else None in
           match config.dir_behavior with
           | Index | Index_or_lists when
               Sys.file_exists (full_path // "index.html") ->
             (* redirect using path, not full path *)
             let new_path = "/" // path // "index.html" in
             S._debug (fun k->k "redirect to `%s`" new_path);
             S.Response.make_raw ~code:301 ""
               ~headers:S.Headers.(empty |> set "location" new_path)
           | Lists | Index_or_lists ->
             let body = html_list_dir ~top:dir path ~parent in
             S.Response.make_string
               ~headers:[header_html; "ETag", Lazy.force mtime]
               (Ok body)
           | Forbidden | Index ->
             S.Response.make_raw ~code:405 "listing dir not allowed"
         ) else (
           try
             let ic = open_in full_path in
             let mime_type =
               if Filename.extension full_path = ".css" then (
                 ["Content-Type", "text/css"]
               ) else if Filename.extension full_path = ".js" then (
                 ["Content-Type", "text/javascript"]
               ) else try
                   let p = Unix.open_process_in (Printf.sprintf "file -i -b %S" full_path) in
                   finally_ ~h:(fun p->ignore @@ Unix.close_process_in p) p
                     (fun p ->
                        try ["Content-Type", String.trim (input_line p)]
                        with _ -> [])
                 with _ -> []
             in
             S.Response.make_raw_stream
               ~headers:(mime_type@["Etag", Lazy.force mtime])
               ~code:200 (S.Byte_stream.of_chan ic)
           with e ->
             S.Response.fail ~code:500 "error while reading file: %s" (Printexc.to_string e))
      )
  ) else (
    S.add_route_handler server ~meth:`GET
      S.Route.(exact_path prefix (string @/ return))
      (fun _ _  -> S.Response.make_raw ~code:405 "download not allowed");
  );
