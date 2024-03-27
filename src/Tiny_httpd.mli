(** Tiny Http Server

    This library implements a very simple, basic HTTP/1.1 server using blocking
    IOs and threads. Basic routing based is provided for convenience,
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

module Buf = Buf

(** {2 IO Abstraction} *)

module IO = Tiny_httpd_core.IO

(** {2 Time} *)

module Time = Time

(** {2 Logging *)

module Log = Tiny_httpd_core.Log

(** {2 Utils} *)

module Util = Tiny_httpd_core.Util

(** {2 Resource pool} *)

module Pool = Tiny_httpd_core.Pool

(** {2 Static directory serving} *)

module Dir = Tiny_httpd_unix.Dir

module type VFS = Tiny_httpd_unix.Dir.VFS

(** {2 HTML combinators} *)

module Html = Tiny_httpd_html
(** Alias to {!Tiny_httpd_html}
    @since 0.12 *)

(** {2 Main server types} *)

module Request = Tiny_httpd_core.Request
module Response = Tiny_httpd_core.Response
module Response_code = Tiny_httpd_core.Response_code
module Route = Tiny_httpd_core.Route
module Headers = Tiny_httpd_core.Headers
module Meth = Tiny_httpd_core.Meth
module Server = Tiny_httpd_core.Server

(** @inline *)
include module type of struct
  include Server
end

val create :
  ?masksigpipe:bool ->
  ?max_connections:int ->
  ?timeout:float ->
  ?buf_size:int ->
  ?get_time_s:(unit -> float) ->
  ?new_thread:((unit -> unit) -> unit) ->
  ?addr:string ->
  ?port:int ->
  ?sock:Unix.file_descr ->
  ?middlewares:([ `Encoding | `Stage of int ] * Middleware.t) list ->
  unit ->
  t
(** Create a new webserver using UNIX abstractions.

    The server will not do anything until {!run} is called on it.
    Before starting the server, one can use {!add_path_handler} and
    {!set_top_handler} to specify how to handle incoming requests.

    @param masksigpipe if true, block the signal {!Sys.sigpipe} which otherwise
    tends to kill client threads when they try to write on broken sockets. Default: [true].

    @param buf_size size for buffers (since 0.11)

    @param new_thread a function used to spawn a new thread to handle a
    new client connection. By default it is {!Thread.create} but one
    could use a thread pool instead.
    See for example {{: https://github.com/c-cube/tiny-httpd-moonpool-bench/blob/0dcbbffb4fe34ea4ad79d46343ad0cebb69ca69f/examples/t1.ml#L31}
      this use of moonpool}.

    @param middlewares see {!add_middleware} for more details.

    @param max_connections maximum number of simultaneous connections.
    @param timeout connection is closed if the socket does not do read or
      write for the amount of second. Default: 0.0 which means no timeout.
      timeout is not recommended when using proxy.
    @param addr address (IPv4 or IPv6) to listen on. Default ["127.0.0.1"].
    @param port to listen on. Default [8080].
    @param sock an existing socket given to the server to listen on, e.g. by
      systemd on Linux (or launchd on macOS). If passed in, this socket will be
      used instead of the [addr] and [port]. If not passed in, those will be
      used. This parameter exists since 0.10.

    @param get_time_s obtain the current timestamp in seconds.
      This parameter exists since 0.11.
*)
