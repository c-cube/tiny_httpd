module MFD = Tiny_httpd_multipart_form_data

let spf = Printf.sprintf
let pf = Printf.printf

let read_stream (st : MFD.st) : _ list =
  let l = ref [] in
  let buffer = Buffer.create 32 in
  let rec loop () =
    match MFD.next st with
    | Part headers ->
      if Buffer.length buffer > 0 then l := `Str (Buffer.contents buffer) :: !l;
      Buffer.clear buffer;
      l := `Part headers :: !l;
      loop ()
    | Read sl ->
      Buffer.add_subbytes buffer sl.bytes sl.off sl.len;
      loop ()
    | End_of_input ->
      if Buffer.length buffer > 0 then l := `Str (Buffer.contents buffer) :: !l;
      l := `End_of_input :: !l;
      List.rev !l
  in
  loop ()

let test input_str =
  let st =
    MFD.create ~buf_size:16 ~boundary:"YOLO" (Iostream.In.of_string input_str)
  in
  let chunks = read_stream st in
  List.iter
    (function
      | `End_of_input -> pf "end of input\n"
      | `Part hs ->
        pf "part [%s]\n"
          (String.concat ";" @@ List.map (fun (k, v) -> spf "%S:%S" k v) hs)
      | `Str s -> pf "chunk %S\n" s)
    chunks;
  ()

let () =
  pf "T1\n";
  test
    "\r\n\
     --YOLO\r\n\
     some-super-cool: header here\r\n\
     ohlook: here\r\n\
     \r\n\
     and now for the b-o-d-y 👏\n\
     \r\n\
     --YOLO\r\n\
     more: headers\r\n\
     \r\n\
     and another body\r\n\
     \r\n\
     --YOLO--";
  pf "T1\n";
  test
    (spf
       "\r\n\
        --YOLO\r\n\
        some-super-cool: header here\r\n\
        ohlook: here\r\n\
        \r\n\
        and now for the bigger body:\n\
        %s\n\
        \r\n\
        --YOLO\r\n\
        more: headers\r\n\
        \r\n\
        and another body\r\n\
        --YOLO--"
       (String.make 500 'a'));
  ()
