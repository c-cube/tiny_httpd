(** Tiny httpd EIO backend.

    This replaces the threads + Unix blocking syscalls of {!Tiny_httpd_server}
    with an Eio-based cooperative system.

    {b NOTE}: this is very experimental and will absolutely change over time,
    especially since Eio itself is also subject to change.
    @since NEXT_RELEASE *)

(* TODO: pass in a switch *)

type 'a with_args =
  ?addr:string ->
  ?port:int ->
  ?unix_sock:string ->
  ?max_connections:int ->
  ?max_buf_pool_size:int ->
  stdenv:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  'a

val io_backend : (unit -> (module Tiny_httpd.Server.IO_BACKEND)) with_args
(** Create a server *)

val create :
  (?buf_size:int ->
  ?middlewares:([ `Encoding | `Stage of int ] * Tiny_httpd.Middleware.t) list ->
  unit ->
  Tiny_httpd.Server.t)
  with_args
(** Create a server *)
