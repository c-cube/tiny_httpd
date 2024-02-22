module S = Tiny_httpd_server
module U = Tiny_httpd_util
module Html = Tiny_httpd_html
module Log = Tiny_httpd_log

type dir_behavior = Index | Lists | Index_or_lists | Forbidden
type hidden = unit

type config = {
  mutable download: bool;
  mutable dir_behavior: dir_behavior;
  mutable delete: bool;
  mutable upload: bool;
  mutable max_upload_size: int;
  _rest: hidden;
}

let default_config_ : config =
  {
    download = true;
    dir_behavior = Forbidden;
    delete = false;
    upload = false;
    max_upload_size = 10 * 1024 * 1024;
    _rest = ();
  }

let default_config () = default_config_

let config ?(download = default_config_.download)
    ?(dir_behavior = default_config_.dir_behavior)
    ?(delete = default_config_.delete) ?(upload = default_config_.upload)
    ?(max_upload_size = default_config_.max_upload_size) () : config =
  { download; dir_behavior; delete; upload; max_upload_size; _rest = () }

let contains_dot_dot s =
  try
    String.iteri
      (fun i c ->
        if c = '.' && i + 1 < String.length s && String.get s (i + 1) = '.' then
          raise Exit)
      s;
    false
  with Exit -> true

(* Human readable size *)
let human_size (x : int) : string =
  if x >= 1_000_000_000 then
    Printf.sprintf "%d.%dG" (x / 1_000_000_000) (x / 1_000_000 mod 1_000_000)
  else if x >= 1_000_000 then
    Printf.sprintf "%d.%dM" (x / 1_000_000) (x / 1000 mod 1_000)
  else if x >= 1_000 then
    Printf.sprintf "%d.%dk" (x / 1000) (x / 100 mod 100)
  else
    Printf.sprintf "%db" x

let header_html = "Content-Type", "text/html"
let ( // ) = Filename.concat

let encode_path s =
  U.percent_encode
    ~skip:(function
      | '/' -> true
      | _ -> false)
    s

let _decode_path s =
  match U.percent_decode s with
  | Some s -> s
  | None -> s

let is_hidden s = String.length s > 0 && s.[0] = '.'

module type VFS = sig
  val descr : string
  val is_directory : string -> bool
  val contains : string -> bool
  val list_dir : string -> string array
  val delete : string -> unit
  val create : string -> (bytes -> int -> int -> unit) * (unit -> unit)
  val read_file_content : string -> Tiny_httpd_stream.t
  val file_size : string -> int option
  val file_mtime : string -> float option
end

type vfs = (module VFS)

let vfs_of_dir (top : string) : vfs =
  let module M = struct
    let descr = top
    let ( // ) = Filename.concat
    let is_directory f = Sys.is_directory (top // f)
    let contains f = Sys.file_exists (top // f)
    let list_dir f = Sys.readdir (top // f)

    let read_file_content f =
      let ic = Unix.(openfile (top // f) [ O_RDONLY ] 0) in
      Tiny_httpd_stream.of_fd_close_noerr ic

    let create f =
      let oc = open_out_bin (top // f) in
      let write = output oc in
      let close () = close_out oc in
      write, close

    let delete f = Sys.remove (top // f)

    let file_size f =
      try Some (Unix.stat (top // f)).Unix.st_size with _ -> None

    let file_mtime f =
      try Some (Unix.stat (top // f)).Unix.st_mtime with _ -> None
  end in
  (module M)

let html_list_dir (module VFS : VFS) ~prefix ~parent d : Html.elt =
  let entries = VFS.list_dir d in
  Array.sort String.compare entries;
  let open Html in
  (* TODO: breadcrumbs for the path, each element a link to the given ancestor dir *)
  let head =
    head []
      [
        title [] [ txtf "list directory %S" VFS.descr ];
        meta [ A.charset "utf-8" ];
      ]
  in

  let n_hidden = ref 0 in
  Array.iter (fun f -> if is_hidden f then incr n_hidden) entries;

  let file_to_elt f : elt option =
    if not @@ contains_dot_dot (d // f) then (
      let fpath = d // f in
      if not @@ VFS.contains fpath then
        Some (li [] [ txtf "%s [invalid file]" f ])
      else (
        let size =
          match VFS.file_size fpath with
          | Some f -> Printf.sprintf " (%s)" @@ human_size f
          | None -> ""
        in
        Some
          (li' []
             [
               sub_e @@ a [ A.href ("/" // prefix // fpath) ] [ txt f ];
               (if VFS.is_directory fpath then
                 sub_e @@ txt "[dir]"
               else
                 sub_empty);
               sub_e @@ txt size;
             ])
      )
    ) else
      None
  in

  let body =
    body' []
      [
        sub_e @@ h2 [] [ txtf "Index of %S" d ];
        (match parent with
        | None -> sub_empty
        | Some p ->
          sub_e
          @@ a
               [ A.href (encode_path ("/" // prefix // p)) ]
               [ txt "(parent directory)" ]);
        sub_e
        @@ ul' []
             [
               (if !n_hidden > 0 then
                 sub_e
                 @@ details' []
                      [
                        sub_e
                        @@ summary [] [ txtf "(%d hidden files)" !n_hidden ];
                        sub_seq
                          (seq_of_array entries
                          |> Seq.filter_map (fun f ->
                                 if is_hidden f then
                                   file_to_elt f
                                 else
                                   None));
                      ]
               else
                 sub_empty);
               sub_seq
                 (seq_of_array entries
                 |> Seq.filter_map (fun f ->
                        if not (is_hidden f) then
                          file_to_elt f
                        else
                          None));
             ];
      ]
  in
  html [] [ head; body ]

let finally_ ~h x f =
  try
    let y = f x in
    h x;
    y
  with e ->
    h x;
    raise e

(* @param on_fs: if true, we assume the file exists on the FS *)
let add_vfs_ ~on_fs ~top ~config ~vfs:((module VFS : VFS) as vfs) ~prefix server
    : unit =
  let route () =
    if prefix = "" then
      S.Route.rest_of_path_urlencoded
    else
      S.Route.exact_path prefix S.Route.rest_of_path_urlencoded
  in
  if config.delete then
    S.add_route_handler server ~meth:`DELETE (route ()) (fun path _req ->
        if contains_dot_dot path then
          S.Response.fail_raise ~code:403 "invalid path in delete"
        else
          S.Response.make_string
            (try
               VFS.delete path;
               Ok "file deleted successfully"
             with e -> Error (500, Printexc.to_string e)))
  else
    S.add_route_handler server ~meth:`DELETE (route ()) (fun _ _ ->
        S.Response.make_raw ~code:405 "delete not allowed");

  if config.upload then
    S.add_route_handler_stream server ~meth:`PUT (route ())
      ~accept:(fun req ->
        match S.Request.get_header_int req "Content-Length" with
        | Some n when n > config.max_upload_size ->
          Error
            (403, "max upload size is " ^ string_of_int config.max_upload_size)
        | Some _ when contains_dot_dot req.S.Request.path ->
          Error (403, "invalid path (contains '..')")
        | _ -> Ok ())
      (fun path req ->
        let write, close =
          try VFS.create path
          with e ->
            S.Response.fail_raise ~code:403 "cannot upload to %S: %s" path
              (Printexc.to_string e)
        in
        let req =
          S.Request.limit_body_size ~max_size:config.max_upload_size req
        in
        Tiny_httpd_stream.iter write req.S.Request.body;
        close ();
        Log.debug (fun k -> k "dir: done uploading file to %S" path);
        S.Response.make_raw ~code:201 "upload successful")
  else
    S.add_route_handler server ~meth:`PUT (route ()) (fun _ _ ->
        S.Response.make_raw ~code:405 "upload not allowed");

  if config.download then
    S.add_route_handler server ~meth:`GET (route ()) (fun path req ->
        Log.debug (fun k -> k "dir: download path=%S" path);
        let mtime =
          lazy
            (match VFS.file_mtime path with
            | None -> S.Response.fail_raise ~code:403 "Cannot access file"
            | Some t -> Printf.sprintf "mtime: %.4f" t)
        in
        if contains_dot_dot path then
          S.Response.fail ~code:403 "Path is forbidden"
        else if not (VFS.contains path) then
          S.Response.fail ~code:404 "File not found"
        else if
          S.Request.get_header req "If-None-Match" = Some (Lazy.force mtime)
        then (
          Log.debug (fun k ->
              k "dir: cached object %S (etag: %S)" path (Lazy.force mtime));
          S.Response.make_raw ~code:304 ""
        ) else if VFS.is_directory path then (
          Log.debug (fun k -> k "dir: list dir %S (topdir %S)" path VFS.descr);
          let parent = Filename.(dirname path) in
          let parent =
            if Filename.basename path <> "." then
              Some parent
            else
              None
          in
          match config.dir_behavior with
          | (Index | Index_or_lists) when VFS.contains (path // "index.html") ->
            (* redirect using path, not full path *)
            let new_path = "/" // prefix // path // "index.html" in
            Log.debug (fun k -> k "dir: redirect to `%s`" new_path);
            S.Response.make_void ~code:301 ()
              ~headers:S.Headers.(empty |> set "location" new_path)
          | Lists | Index_or_lists ->
            let body =
              html_list_dir ~prefix vfs path ~parent |> Html.to_string_top
            in
            S.Response.make_string
              ~headers:[ header_html; "ETag", Lazy.force mtime ]
              (Ok body)
          | Forbidden | Index ->
            S.Response.make_raw ~code:405 "listing dir not allowed"
        ) else (
          try
            let mime_type =
              if Filename.extension path = ".css" then
                [ "Content-Type", "text/css" ]
              else if Filename.extension path = ".js" then
                [ "Content-Type", "text/javascript" ]
              else if on_fs then (
                (* call "file" util *)
                let ty = Mime_.mime_of_path (top // path) in
                [ "content-type", ty ]
              ) else
                []
            in
            let stream = VFS.read_file_content path in
            S.Response.make_raw_stream
              ~headers:(mime_type @ [ "Etag", Lazy.force mtime ])
              ~code:200 stream
          with e ->
            let bt = Printexc.get_raw_backtrace () in
            let msg = Printexc.to_string e in
            Log.error (fun k ->
                k "dir.get failed: %s@.%s" msg
                  (Printexc.raw_backtrace_to_string bt));
            S.Response.fail ~code:500 "error while reading file: %s" msg
        ))
  else
    S.add_route_handler server ~meth:`GET (route ()) (fun _ _ ->
        S.Response.make_raw ~code:405 "download not allowed");
  ()

let add_vfs ~config ~vfs ~prefix server : unit =
  add_vfs_ ~on_fs:false ~top:"." ~config ~prefix ~vfs server

let add_dir_path ~config ~dir ~prefix server : unit =
  add_vfs_ ~on_fs:true ~top:dir ~config ~prefix ~vfs:(vfs_of_dir dir) server

module Embedded_fs = struct
  module Str_map = Map.Make (String)

  type t = { mtime: float; mutable entries: entry Str_map.t }
  and entry = File of { content: string; mtime: float } | Dir of t

  let create ?(mtime = Unix.gettimeofday ()) () : t =
    { mtime; entries = Str_map.empty }

  let split_path_ (path : string) : string list * string =
    let basename = Filename.basename path in
    let dirname =
      Filename.dirname path |> String.split_on_char '/'
      |> List.filter (function
           | "" | "." -> false
           | _ -> true)
    in
    dirname, basename

  let add_file ?mtime (self : t) ~path content : unit =
    let mtime =
      match mtime with
      | Some t -> t
      | None -> self.mtime
    in
    let dir_path, basename = split_path_ path in
    if List.mem ".." dir_path then invalid_arg "add_file: '..' is not allowed";

    let rec loop self dir =
      match dir with
      | [] ->
        self.entries <-
          Str_map.add basename (File { mtime; content }) self.entries
      | d :: ds ->
        let sub =
          match Str_map.find d self.entries with
          | Dir sub -> sub
          | File _ ->
            invalid_arg
              (Printf.sprintf "in path %S, %S is a file, not a directory" path d)
          | exception Not_found ->
            let sub = create ~mtime:self.mtime () in
            self.entries <- Str_map.add d (Dir sub) self.entries;
            sub
        in
        loop sub ds
    in
    loop self dir_path

  (* find entry *)
  let find_ self path : entry option =
    let dir_path, basename = split_path_ path in
    let rec loop self dir_name =
      match dir_name with
      | [] -> (try Some (Str_map.find basename self.entries) with _ -> None)
      | d :: ds ->
        (match Str_map.find d self.entries with
        | exception Not_found -> None
        | File _ -> None
        | Dir sub -> loop sub ds)
    in
    if path = "" then
      Some (Dir self)
    else
      loop self dir_path

  let to_vfs self : vfs =
    let module M = struct
      let descr = "Embedded_fs"

      let file_mtime p =
        match find_ self p with
        | Some (File { mtime; _ }) -> Some mtime
        | Some (Dir _) -> Some self.mtime
        | _ -> None

      let file_size p =
        match find_ self p with
        | Some (File { content; _ }) -> Some (String.length content)
        | _ -> None

      let contains p =
        Log.debug (fun k -> k "vfs: contains %S" p);
        match find_ self p with
        | Some _ -> true
        | None -> false

      let is_directory p =
        match find_ self p with
        | Some (Dir _) -> true
        | _ -> false

      let read_file_content p =
        match find_ self p with
        | Some (File { content; _ }) -> Tiny_httpd_stream.of_string content
        | _ -> failwith (Printf.sprintf "no such file: %S" p)

      let list_dir p =
        Log.debug (fun k -> k "vfs: list dir %S" p);
        match find_ self p with
        | Some (Dir sub) ->
          Str_map.fold (fun sub _ acc -> sub :: acc) sub.entries []
          |> Array.of_list
        | _ -> failwith (Printf.sprintf "no such directory: %S" p)

      let create _ = failwith "Embedded_fs is read-only"
      let delete _ = failwith "Embedded_fs is read-only"
    end in
    (module M)
end
