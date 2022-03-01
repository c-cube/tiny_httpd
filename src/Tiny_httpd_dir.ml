module S = Tiny_httpd
module U = Tiny_httpd_util
module Pf = Printf

type dir_behavior =
  | Index | Lists | Index_or_lists | Forbidden

type hidden = unit
type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
  _rest: hidden
}

let default_config_ : config =
  { download=true;
    dir_behavior=Forbidden;
    delete=false;
    upload=false;
    max_upload_size = 10 * 1024 * 1024;
    _rest=();
  }

let default_config () = default_config_
let config
    ?(download=default_config_.download)
    ?(dir_behavior=default_config_.dir_behavior)
    ?(delete=default_config_.delete)
    ?(upload=default_config_.upload)
    ?(max_upload_size=default_config_.max_upload_size)
    () : config =
  { download; dir_behavior; delete; upload; max_upload_size;
    _rest=()}

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

module type VFS = sig
  val descr : string
  val is_directory : string -> bool
  val contains : string -> bool
  val list_dir : string -> string array
  val delete : string -> unit
  val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
  val read_file_content : string -> Tiny_httpd.Byte_stream.t
  val file_size : string -> int option
  val file_mtime : string -> float option
end

type vfs = (module VFS)

let vfs_of_dir (top:string) : vfs =
  let module M = struct
    let descr = top
    let (//) = Filename.concat
    let is_directory f = Sys.is_directory (top // f)
    let contains f = Sys.file_exists (top // f)
    let list_dir f = Sys.readdir (top // f)
    let read_file_content f =
      let ic = open_in_bin (top // f) in
      S.Byte_stream.of_chan ic
    let create f =
      let oc = open_out_bin (top // f) in
      let write = output oc in
      let close() = close_out oc in
      write, close
    let delete f = Sys.remove (top // f)
    let file_size f =
      try Some (Unix.stat (top // f)).Unix.st_size
      with _ -> None
    let file_mtime f =
      try Some (Unix.stat (top // f)).Unix.st_mtime
      with _ -> None
  end in
  (module M)

let html_list_dir (module VFS:VFS) ~parent d : string =
  let entries = VFS.list_dir d in
  Array.sort compare entries;
  let body = Buffer.create 256 in
  (* TODO: breadcrumbs for the path, each element a link to the given ancestor dir *)
  Printf.bprintf body {|<head><title> http_of_dir %S</title><meta charset="utf-8">
  </head><body>
    <h2> Index of %S</h2>
  |} VFS.descr d;
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
         let fpath = d // f in
         if not @@ VFS.contains fpath then (
           Printf.bprintf body "  <li> %s [invalid file]</li>\n" f
         ) else (
           let size =
             match VFS.file_size fpath with
             | Some f -> Printf.sprintf " (%s)" @@ human_size f
             | None -> ""
           in
           Printf.bprintf body "  <li> <a href=\"/%s\"> %s </a> %s%s </li>\n"
             (encode_path (d // f)) f (if VFS.is_directory fpath then "[dir]" else "") size
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

(* @param on_fs: if true, we assume the file exists on the FS *)
let add_vfs_ ~on_fs ~top ~config ~vfs:((module VFS:VFS) as vfs) ~prefix server : unit=

  if config.delete then (
    S.add_route_handler server ~meth:`DELETE
      S.Route.(exact_path prefix (rest_of_path_urlencoded))
      (fun path _req ->
         if contains_dot_dot path then (
           S.Response.fail_raise ~code:403 "invalid path in delete"
         ) else (
           S.Response.make_string
             (try
                VFS.delete path; Ok "file deleted successfully"
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
         let write, close =
           try VFS.create path
           with e ->
             S.Response.fail_raise ~code:403 "cannot upload to %S: %s"
               path (Printexc.to_string e)
         in
         let req = S.Request.limit_body_size ~max_size:config.max_upload_size req in
         S.Byte_stream.iter write req.S.Request.body;
         close ();
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
         let mtime = lazy (
           match VFS.file_mtime path with
           | None -> S.Response.fail_raise ~code:403 "Cannot access file"
           | Some t -> Printf.sprintf "mtime: %.4f" t
         ) in
         if contains_dot_dot path then (
           S.Response.fail ~code:403 "Path is forbidden";
         ) else if not (VFS.contains path) then (
           S.Response.fail ~code:404 "File not found";
         ) else if S.Request.get_header req "If-None-Match" = Some (Lazy.force mtime) then (
           S._debug (fun k->k "cached object %S (etag: %S)" path (Lazy.force mtime));
           S.Response.make_raw ~code:304 ""
         ) else if VFS.is_directory path then (
           S._debug (fun k->k "list dir %S (topdir %S)" path VFS.descr);
           let parent = Filename.(dirname path) in
           let parent = if parent <> path then Some parent else None in
           match config.dir_behavior with
           | Index | Index_or_lists when VFS.contains (path // "index.html") ->
             (* redirect using path, not full path *)
             let new_path = "/" // path // "index.html" in
             S._debug (fun k->k "redirect to `%s`" new_path);
             S.Response.make_raw ~code:301 ""
               ~headers:S.Headers.(empty |> set "location" new_path)
           | Lists | Index_or_lists ->
             let body = html_list_dir vfs path ~parent in
             S.Response.make_string
               ~headers:[header_html; "ETag", Lazy.force mtime]
               (Ok body)
           | Forbidden | Index ->
             S.Response.make_raw ~code:405 "listing dir not allowed"
         ) else (
           try
             let mime_type =
               if Filename.extension path = ".css" then (
                 ["Content-Type", "text/css"]
               ) else if Filename.extension path = ".js" then (
                 ["Content-Type", "text/javascript"]
               ) else if on_fs then (
                 (* call "file" util *)
                 try
                   let p = Unix.open_process_in (Printf.sprintf "file -i -b %S" (top // path)) in
                   finally_ ~h:(fun p->ignore @@ Unix.close_process_in p) p
                     (fun p ->
                        try ["Content-Type", String.trim (input_line p)]
                        with _ -> [])
                 with _ -> []
               ) else []
             in
             let stream = VFS.read_file_content path in
             S.Response.make_raw_stream
               ~headers:(mime_type@["Etag", Lazy.force mtime])
               ~code:200 stream
           with e ->
             S.Response.fail ~code:500 "error while reading file: %s" (Printexc.to_string e))
      )
  ) else (
    S.add_route_handler server ~meth:`GET
      S.Route.(exact_path prefix (string @/ return))
      (fun _ _  -> S.Response.make_raw ~code:405 "download not allowed");
  );
  ()

let add_vfs ~config ~vfs ~prefix server : unit =
  add_vfs_ ~on_fs:false ~top:"." ~config ~prefix ~vfs server

let add_dir_path ~config ~dir ~prefix server : unit =
  add_vfs_ ~on_fs:true ~top:dir ~config ~prefix ~vfs:(vfs_of_dir dir) server

module Embedded_fs = struct
  module Str_tbl = Hashtbl.Make(struct
      include String
      let hash = Hashtbl.hash
    end)

  type t = {
    entries: entry Str_tbl.t
  } [@@unboxed]

  and entry =
    | File of {
        content: string;
      }
    | Dir of t

  (* TODO: the rest *)
  (* TODO: use util.split_on_slash *)


end

