module MFD = Tiny_httpd_multipart_form_data

let pf = Printf.printf
let spf = Printf.sprintf

let pp_headers hs =
  spf "[%s]" (String.concat ";" @@ List.map (fun (k, v) -> spf "%S: %S" k v) hs)

let test_headers h (exp : _ option) =
  match MFD.Content_disposition.parse h, exp with
  | Some c1, Some c2 ->
    pf "h %s: got %s, expected %s, same=%b\n" (pp_headers h)
      (MFD.Content_disposition.to_string c1)
      (MFD.Content_disposition.to_string c2)
      (c1 = c2)
  | Some c1, None ->
    pf "h: %s, unexpected content disp %s\n" (pp_headers h)
      (MFD.Content_disposition.to_string c1)
  | None, Some c2 ->
    pf "h: %s, expected content disp %s\n" (pp_headers h)
      (MFD.Content_disposition.to_string c2)
  | None, None -> pf "h: %s, no content disp\n" (pp_headers h)

let () =
  test_headers [ "content-foobar", "yolo"; "other", "whatev" ] None;
  test_headers
    [
      "content-disposition", "form-data; name=helloworld; junk";
      "other", "whatev";
    ]
    (Some { kind = "form-data"; name = Some "helloworld"; filename = None });
  test_headers
    [
      ( "content-disposition",
        "form-data; lol=mdr; filename=\"some quoted stuff\"" );
    ]
    (Some
       { kind = "form-data"; name = None; filename = Some "some quoted stuff" });
  ()
