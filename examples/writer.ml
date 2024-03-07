module H = Tiny_httpd
open Tiny_httpd_core

let serve_zeroes server : unit =
  Server.add_route_handler server Route.(exact "zeroes" @/ int @/ return)
  @@ fun n _req ->
  (* stream [n] zeroes *)
  let write (oc : H.IO.Output.t) : unit =
    let buf = Bytes.make 1 '0' in
    for _i = 1 to n do
      H.IO.Output.output oc buf 0 1
    done
  in
  let writer = H.IO.Writer.make ~write () in
  Response.make_writer @@ Ok writer

let serve_file server : unit =
  H.add_route_handler server H.(Route.(exact "file" @/ string @/ return))
  @@ fun file _req ->
  if Sys.file_exists file then (
    (* stream the content of the file *)
    let write oc =
      let buf = Bytes.create 4096 in
      let ic = open_in file in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      while
        let n = input ic buf 0 (Bytes.length buf) in
        if n > 0 then H.IO.Output.output oc buf 0 n;
        n > 0
      do
        ()
      done
    in

    let writer = H.IO.Writer.make ~write () in
    Response.make_writer @@ Ok writer
  ) else
    Response.fail ~code:404 "file not found"

let () =
  let port = ref 8085 in
  Arg.parse [ "-p", Arg.Set_int port, " port" ] ignore "";
  let server = H.create ~port:!port () in
  Printf.printf "listen on http://localhost:%d/\n%!" !port;
  serve_file server;
  serve_zeroes server;
  H.add_route_handler server Route.return (fun _req ->
      let body =
        H.Html.(
          div []
            [
              p [] [ txt "routes" ];
              ul []
                [
                  li []
                    [ a [ A.href "/zeroes/1000" ] [ txt "get 1000 zeroes" ] ];
                  li [] [ a [ A.href "/file/f_13M" ] [ txt "read file" ] ];
                ];
            ])
        |> H.Html.to_string_top
      in
      Response.make_string @@ Ok body);
  H.run_exn server
