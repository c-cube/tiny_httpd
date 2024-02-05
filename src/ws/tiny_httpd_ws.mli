open Common_
open Tiny_httpd_server
module IO = Tiny_httpd_io

(* FIXME: also pass client address to the handler *)

type handler = IO.Input.t -> IO.Output.t -> unit
(** Websocket handler *)

val add_route_handler :
  ?accept:(unit Request.t -> (unit, int * string) result) ->
  ?accept_ws_protocol:(string -> bool) ->
  Tiny_httpd_server.t ->
  (upgrade_handler, upgrade_handler) Route.t ->
  handler ->
  unit
(** Add a route handler for a websocket endpoint.
    @param accept_ws_protocol decides whether this endpoint accepts the websocket protocol
    sent by the client. Default accepts everything. *)
