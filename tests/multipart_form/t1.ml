module MFD = Tiny_httpd_multipart_form_data

let spf = Printf.sprintf
let pf = Printf.printf

let read_stream (st : MFD.st) : _ list =
  let l = ref [] in
  let buf = Bytes.create 12 in
  let buffer = Buffer.create 32 in
  let rec loop () =
    match MFD.Private_.read_chunk_ st buf 0 (Bytes.length buf) with
    | Delim ->
      if Buffer.length buffer > 0 then l := `Str (Buffer.contents buffer) :: !l;
      Buffer.clear buffer;
      l := `Delim :: !l;
      loop ()
    | Read n ->
      Buffer.add_subbytes buffer buf 0 n;
      loop ()
    | Eof ->
      if Buffer.length buffer > 0 then l := `Str (Buffer.contents buffer) :: !l;
      List.rev !l
  in
  loop ()

let test input_str =
  let st =
    MFD.create ~buf_size:16 ~delim:"YOLO" (Iostream.In.of_string input_str)
  in
  let chunks = read_stream st in
  List.iter
    (function
      | `Delim -> pf "delim\n"
      | `Str s -> pf "chunk %S\n" s)
    chunks;
  ()

let () =
  pf "T1\n";
  test
    {|hello--YOLO
  world
    what is the meaning of--YOLOthis??--YOLOok ok ok--YOLO|};
  pf "T2\n";
  test "--YOLO--YOLOah bon--YOLOaight--YOLO--YOLO";
  pf "T3\n";
  test
    (spf "--YOLO%s--YOLO--YOLO%s--YOLO%s" (String.make 400 'a')
       (String.make 400 'b') (String.make 400 'c'));
  ()
