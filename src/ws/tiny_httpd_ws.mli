(** Websockets for Tiny_httpd.

    This sub-library ([tiny_httpd.ws]) exports a small implementation for a
    websocket server. It has no additional dependencies. *)

(** Synchronization primitive used to allow both the reader to reply to "ping",
    and the handler to send messages, without stepping on each other's toes.

    @since NEXT_RELEASE *)
module With_lock : sig
  type t = { with_lock: 'a. (unit -> 'a) -> 'a }
  (** A primitive to run the callback in a critical section where others cannot
      run at the same time.

      The default is a mutex, but that works poorly with thread pools so it's
      possible to use a semaphore or a cooperative mutex instead. *)

  type builder = unit -> t

  val default_builder : builder
  (** Lock using [Mutex]. *)
end

type handler = unit Request.t -> IO.Input.t -> IO.Output.t -> unit
(** Websocket handler *)

val upgrade :
  ?with_lock:With_lock.t ->
  IO.Input.t ->
  IO.Output.t ->
  IO.Input.t * IO.Output.t
(** Upgrade a byte stream to the websocket framing protocol.
    @param with_lock
      if provided, use this to prevent reader and writer to compete on sending
      frames. since NEXT_RELEASE. *)

exception Close_connection
(** Exception that can be raised from IOs inside the handler, when the
    connection is closed from underneath. *)

val add_route_handler :
  ?accept:(unit Request.t -> (unit, int * string) result) ->
  ?accept_ws_protocol:(string -> bool) ->
  ?middlewares:Server.Head_middleware.t list ->
  ?with_lock:With_lock.builder ->
  Server.t ->
  (Server.upgrade_handler, Server.upgrade_handler) Route.t ->
  handler ->
  unit
(** Add a route handler for a websocket endpoint.
    @param accept_ws_protocol
      decides whether this endpoint accepts the websocket protocol sent by the
      client. Default accepts everything.
    @param with_lock
      if provided, use this to synchronize writes between the frame reader
      (replies "pong" to "ping") and the handler emitting writes. since
      NEXT_RELEASE. *)

(**/**)

module Private_ : sig
  val apply_masking :
    mask_key:bytes -> mask_offset:int -> bytes -> int -> int -> unit
end

(**/**)
