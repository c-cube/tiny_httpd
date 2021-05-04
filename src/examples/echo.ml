
module S = Tiny_httpd

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of sitting by \
   her sister on the bank, and of having nothing to do: once or twice she had peeped \
   into the book her sister was reading, but it had no pictures or conversations in it, \
   <and what is the use of a book,> thought Alice <without pictures or conversations?> \
   So she was considering in her own mind (as well as she could, for the hot day made \
   her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would \
   be worth the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; \
   nor did Alice think it so very much out of the way to hear the Rabbit say to itself, \
   <Oh dear! Oh dear! I shall be late!> (when she thought it over afterwards, it \
   occurred to her that she ought to have wondered at this, but at the time it all \
   seemed quite natural); but when the Rabbit actually took a watch out of its \
   waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, \
   for it flashed across her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran \
   across the field after it, and fortunately was just in time to see it pop down a \
   large rabbit-hole under the hedge. In another moment down went Alice after it, never \
   once considering how in the world she was to get out again. The rabbit-hole went \
   straight on like a tunnel for some way, and then dipped suddenly down, so suddenly \
   that Alice had not a moment to think about stopping herself before she found herself \
   falling down a very deep well. Either the well was very deep, or she fell very \
   slowly, for she had plenty of time as she went down to look about her and to wonder \
   what was going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the sides of \
   the well, and noticed that they were filled with cupboards......"
;;

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "-p", Arg.Set_int port_, " set port";
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "-j", Arg.Set_int j, " maximum number of connections";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";
  let server = S.create ~port:!port_ ~max_connections:!j () in
  Tiny_httpd_camlzip.setup ~compress_above:1024 ~buf_size:(1024*1024) server;
  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "hello" @/ return)
    (fun _req -> S.Response.make_string (Ok text));
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "zcat" @/ string_urlencoded @/ return)
    (fun path _req ->
        let ic = open_in path in
        let str = S.Byte_stream.of_chan ic in
        let mime_type =
          try
            let p = Unix.open_process_in (Printf.sprintf "file -i -b %S" path) in
            try
              let s = ["Content-Type", String.trim (input_line p)] in
              ignore @@ Unix.close_process_in p;
              s
            with _ -> ignore @@ Unix.close_process_in p; []
          with _ -> []
        in
        S.Response.make_stream ~headers:mime_type (Ok str)
      );
  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        let q =
          S.Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
          |> String.concat ";"
        in
        S.Response.make_string
          (Ok (Format.asprintf "echo:@ %a@ (query: %s)@." S.Request.pp req q)));
  S.add_route_handler_stream ~meth:`PUT server
    S.Route.(exact "upload" @/ string @/ return)
    (fun path req ->
        S._debug (fun k->k "start upload %S, headers:\n%s\n\n%!" path
                     (Format.asprintf "%a" S.Headers.pp (S.Request.headers req)));
        try
          let oc = open_out @@ "/tmp/" ^ path in
          S.Byte_stream.to_chan oc req.S.Request.body;
          flush oc;
          S.Response.make_string (Ok "uploaded file")
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s" (Printexc.to_string e)
      );
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
