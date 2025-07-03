(** Lwt backend for Tiny_httpd.

    This only works on OCaml 5 because it uses effect handlers to use Lwt in
    direct style.

    {b NOTE}: this is very experimental and will absolutely change over time,
    @since NEXT_RELEASE *)

module Task = Task

type 'a with_args =
  ?addr:string ->
  ?port:int ->
  ?unix_sock:string ->
  ?max_connections:int ->
  ?max_buf_pool_size:int ->
  ?buf_size:int ->
  'a

val io_backend : (unit -> (module Tiny_httpd.Server.IO_BACKEND)) with_args
(** Create a server *)

val create :
  (?middlewares:([ `Encoding | `Stage of int ] * Tiny_httpd.Middleware.t) list ->
  unit ->
  Tiny_httpd.Server.t Lwt.t)
  with_args
(** Create a server *)
