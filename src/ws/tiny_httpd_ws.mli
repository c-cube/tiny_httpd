(** Websockets for Tiny_httpd.

    This sub-library ([tiny_httpd.ws]) exports a small implementation for a
    websocket server. It has no additional dependencies. *)

type handler = unit Request.t -> IO.Input.t -> IO.Output.t -> unit
(** Websocket handler *)

val upgrade : IO.Input.t -> IO.Output.t -> IO.Input.t * IO.Output.t
(** Upgrade a byte stream to the websocket framing protocol. *)

exception Close_connection
(** Exception that can be raised from IOs inside the handler, when the
    connection is closed from underneath. *)

val add_route_handler :
  ?accept:(unit Request.t -> (unit, int * string) result) ->
  ?accept_ws_protocol:(string -> bool) ->
  ?middlewares:Server.Head_middleware.t list ->
  Server.t ->
  (Server.upgrade_handler, Server.upgrade_handler) Route.t ->
  handler ->
  unit
(** Add a route handler for a websocket endpoint.
    @param accept_ws_protocol
      decides whether this endpoint accepts the websocket protocol sent by the
      client. Default accepts everything. *)

(**/**)

module Private_ : sig
  val apply_masking :
    mask_key:bytes -> mask_offset:int -> bytes -> int -> int -> unit
end

(**/**)
