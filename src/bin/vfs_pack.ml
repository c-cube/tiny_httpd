let spf = Printf.sprintf
let fpf = Printf.fprintf
let now_ = Unix.gettimeofday ()
let verbose = ref false

type entry =
  | File of string * string
  | Url of string * string
  | Mirror of string * string
  | Source_file of string

let read_file filename =
  let ic = open_in_bin filename in
  let buf = Buffer.create 32 in
  let b = Bytes.create 1024 in
  while
    let n = input ic b 0 (Bytes.length b) in
    Buffer.add_subbytes buf b 0 n;
    n > 0
  do
    ()
  done;
  close_in ic;
  Buffer.contents buf

let split_comma s = Scanf.sscanf s "%s@,%s" (fun x y -> x, y)

let is_url s =
  let is_prefix pre s =
    String.length s > String.length pre
    && String.sub s 0 (String.length pre) = pre
  in
  is_prefix "http://" s || is_prefix "https://" s

let emit oc (l : entry list) : unit =
  fpf oc "let embedded_fs = Tiny_httpd_dir.Embedded_fs.create ~mtime:%f ()\n"
    now_;

  let add_vfs ~mtime vfs_path content =
    fpf oc
      "let () = Tiny_httpd_dir.Embedded_fs.add_file embedded_fs \n\
      \  ~mtime:%h ~path:%S\n\
      \  %S\n"
      mtime vfs_path content
  in

  let rec add_entry = function
    | File (vfs_path, actual_path) ->
      if !verbose then
        Printf.eprintf "add file %S = %S\n%!" vfs_path actual_path;

      let content = read_file actual_path in
      let mtime = (Unix.stat actual_path).Unix.st_mtime in
      add_vfs ~mtime vfs_path content
    | Url (vfs_path, url) ->
      if !verbose then Printf.eprintf "add url %S = %S\n%!" vfs_path url;

      (match Curly.get ~args:[ "-L" ] url with
      | Ok b ->
        let code = b.Curly.Response.code in
        if code >= 200 && code < 300 then
          add_vfs ~mtime:now_ vfs_path b.Curly.Response.body
        else
          failwith
            (Printf.sprintf "download of %S failed with code: %d" url code)
      | Error err ->
        failwith
          (Format.asprintf "download of %S failed: %a" url Curly.Error.pp err))
    | Mirror (vfs_path, dir) ->
      if !verbose then
        Printf.eprintf "mirror directory %S as %S\n%!" dir vfs_path;

      let rec traverse rpath =
        let real_path = Filename.concat dir rpath in
        if Sys.is_directory real_path then (
          let arr = Sys.readdir real_path in
          Array.iter (fun e -> traverse (Filename.concat rpath e)) arr
        ) else
          add_entry (File (Filename.concat vfs_path rpath, real_path))
      in
      traverse "."
    | Source_file f ->
      if !verbose then Printf.eprintf "read source file %S\n%!" f;

      let lines =
        read_file f |> String.split_on_char '\n' |> List.map String.trim
        |> List.filter (( <> ) "")
      in

      let process_line line =
        let vfs_path, path = split_comma line in
        if is_url path then
          add_entry (Url (vfs_path, path))
        else
          add_entry (File (vfs_path, path))
      in

      List.iter process_line lines
  in
  List.iter add_entry l;

  fpf oc "let vfs = Tiny_httpd_dir.Embedded_fs.to_vfs embedded_fs\n";
  ()

let help =
  {|vfs-pack [opt]+

Builds an OCaml module containing a `Tiny_httpd_dir.Embedded_fs.t`
virtual file system. This is useful to pack assets into an OCaml binary,
for example.

Each entry in the VFS can be added from the command line using:

--file=foo/bar,actual/path/to/file to add an entry foo/bar in the VFS
  with the content of actual/path/to/file. The mtime of the file is preserved.

--url=foo/bar,https://something.com/ to add an entry foo/bar in the VFS
  with the content of the URL (downloaded using curl).

--mirror=prefix,some/dir/ copies the entire directory into the VFS
  under prefix path "prefix". If prefix is empty, the directory is copied
  directly into the root.

-F=file reads lines from file. Each line is a pair vfs_path,actual_path
and is processed as previously. If actual_path looks like an http(s) URL
it is treated as such.
|}

let () =
  let entries = ref [] in
  let out = ref "" in

  let add_entry e = entries := e :: !entries in

  let add_file s =
    let vfs_path, path = split_comma s in
    add_entry (File (vfs_path, path))
  and add_mirror s =
    let vfs_path, path = split_comma s in
    let vfs_path, path =
      if path = "" then
        "", vfs_path
      else
        vfs_path, path
    in
    add_entry (Mirror (vfs_path, path))
  and add_source f = add_entry (Source_file f)
  and add_url s =
    let vfs_path, path = split_comma s in
    if is_url path then
      add_entry (Url (vfs_path, path))
    else
      invalid_arg (spf "--url: invalid URL %S" path)
  in

  let opts =
    [
      "-v", Arg.Set verbose, " verbose mode";
      "-o", Arg.Set_string out, " set output file";
      "--file", Arg.String add_file, " <name,file> adds name=file to the VFS";
      "--url", Arg.String add_url, " <name,url> adds name=url to the VFS";
      ( "--mirror",
        Arg.String add_mirror,
        " <prefix,dir> copies directory dir into the VFS under prefix" );
      ( "-F",
        Arg.String add_source,
        " <file> reads entries from the file, on per line" );
    ]
    |> Arg.align
  in
  Arg.parse opts (fun _ -> raise (Arg.Help "no positional arg")) help;

  let out, close =
    if !out = "" then
      stdout, ignore
    else
      open_out !out, close_out
  in
  emit out !entries;
  close out;
  exit 0
