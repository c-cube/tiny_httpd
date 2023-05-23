(** {1 Tiny Http Server}

    This library implements a very simple, basic HTTP/1.1 server using blocking
    IOs and threads. Basic routing based on {!Scanf} is provided for convenience,
    so that several handlers can be registered.

    It is possible to use a thread pool, see {!create}'s argument [new_thread].

    The [echo] example (see [src/examples/echo.ml]) demonstrates some of the
    features by declaring a few endpoints, including one for uploading files:

    {[
module S = Tiny_httpd

let () =
  let server = S.create () in

  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "hello" @/ string @/ return)
    (fun name _req -> S.Response.make_string (Ok ("hello " ^name ^"!\n")));

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req -> S.Response.make_string
        (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)));

  (* file upload *)
  S.add_route_handler ~meth:`PUT server
    S.Route.(exact "upload" @/ string_urlencoded @/ return)
    (fun path req ->
        try
          let oc = open_out @@ "/tmp/" ^ path in
          output_string oc req.S.Request.body;
          flush oc;
          S.Response.make_string (Ok "uploaded file")
        with e ->
          S.Response.fail ~code:500 "couldn't upload file: %s"
            (Printexc.to_string e)
      );

  (* run the server *)
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
    ]}

    It is then possible to query it using curl:

    {[
$ dune exec src/examples/echo.exe &
listening on http://127.0.0.1:8080

# the path "hello/name" greets you.
$ curl -X GET http://localhost:8080/hello/quadrarotaphile
hello quadrarotaphile!

# the path "echo" just prints the request.
$ curl -X GET http://localhost:8080/echo --data "howdy y'all"
echo:
{meth=GET;
 headers=Host: localhost:8080
         User-Agent: curl/7.66.0
         Accept: */*
         Content-Length: 10
         Content-Type: application/x-www-form-urlencoded;
 path="/echo"; body="howdy y'all"}


    ]}

*)

(** {2 Tiny buffer implementation}

    These buffers are used to avoid allocating too many byte arrays when
    processing streams and parsing requests.
*)

module Buf = Tiny_httpd_buf

(** {2 Generic stream of data}

    Streams are used to represent a series of bytes that can arrive progressively.
    For example, an uploaded file will be sent as a series of chunks. *)

module Byte_stream = Tiny_httpd_stream

(** {2 Main Server Type} *)

(** @inline *)
include module type of struct
  include Tiny_httpd_server
end

(** {2 Utils} *)

module Util = Tiny_httpd_util

(** {2 Static directory serving} *)

module Dir = Tiny_httpd_dir

module Html = Tiny_httpd_html
(** Alias to {!Tiny_httpd_html}
    @since NEXT_RELEASE *)
