module MFD = Tiny_httpd_multipart_form_data

let pf = Printf.printf
let spf = Printf.sprintf

let pp_headers hs =
  spf "[%s]" (String.concat ";" @@ List.map (fun (k, v) -> spf "%S: %S" k v) hs)

let test_headers h (exp : string option) =
  match MFD.parse_content_type h, exp with
  | Some (`boundary c1), Some c2 ->
    pf "h %s: got %S, expected %S, same=%b\n" (pp_headers h) c1 c2 (c1 = c2)
  | Some (`boundary c1), None ->
    pf "h: %s, unexpected content type %S\n" (pp_headers h) c1
  | None, Some c2 -> pf "h: %s, expected content type %S\n" (pp_headers h) c2
  | None, None -> pf "h: %s, no content type\n" (pp_headers h)

let () =
  test_headers [ "content-type", "yolo"; "other", "whatev" ] None;
  test_headers
    [
      "content-type", "multipart/form-data; boundary=helloworld; junk";
      "other", "whatev";
    ]
    (Some "helloworld");
  test_headers
    [
      ( "content-type",
        "multipart/form-data; lol=mdr;  boundary=\"some quoted boundary\"" );
    ]
    (Some "some quoted boundary");
  ()
