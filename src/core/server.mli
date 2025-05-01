(** HTTP server.

    This module implements a very simple, basic HTTP/1.1 server using blocking
    IOs and threads.

    It is possible to use a thread pool, see {!create}'s argument [new_thread].

    @since 0.13 *)

exception Bad_req of int * string
(** Exception raised to exit request handlers with a code+error message *)

(** {2 Middlewares}

    A middleware can be inserted in a handler to modify or observe its behavior.

    @since 0.11 *)

module Middleware : sig
  type handler = IO.Input.t Request.t -> resp:(Response.t -> unit) -> unit
  (** Handlers are functions returning a response to a request. The response can
      be delayed, hence the use of a continuation as the [resp] parameter. *)

  type t = handler -> handler
  (** A middleware is a handler transformation.

      It takes the existing handler [h], and returns a new one which, given a
      query, modify it or log it before passing it to [h], or fail. It can also
      log or modify or drop the response. *)

  val nil : t
  (** Trivial middleware that does nothing. *)
end

(** A middleware that only considers the request's head+headers.

    These middlewares are simpler than full {!Middleware.t} and work in more
    contexts.
    @since 0.17 *)
module Head_middleware : sig
  type t = { handle: 'a. 'a Request.t -> 'a Request.t }
  (** A handler that takes the request, without its body, and possibly modifies
      it.
      @since 0.17 *)

  val trivial : t
  (** Pass through *)

  val to_middleware : t -> Middleware.t
end

(** {2 Main Server type} *)

type t
(** A HTTP server. See {!create} for more details. *)

(** A backend that provides IO operations, network operations, etc.

    This is used to decouple tiny_httpd from the scheduler/IO library used to
    actually open a TCP server and talk to clients. The classic way is based on
    {!Unix} and blocking IOs, but it's also possible to use an OCaml 5 library
    using effects and non blocking IOs. *)
module type IO_BACKEND = sig
  val init_addr : unit -> string
  (** Initial TCP address *)

  val init_port : unit -> int
  (** Initial port *)

  val get_time_s : unit -> float
  (** Obtain the current timestamp in seconds. *)

  val tcp_server : unit -> IO.TCP_server.builder
  (** TCP server builder, to create servers that can listen on a port and handle
      clients. *)
end

val create_from :
  ?enable_logging:bool ->
  ?buf_size:int ->
  ?head_middlewares:Head_middleware.t list ->
  ?middlewares:([ `Encoding | `Stage of int ] * Middleware.t) list ->
  backend:(module IO_BACKEND) ->
  unit ->
  t
(** Create a new webserver using provided backend.

    The server will not do anything until {!run} is called on it. Before
    starting the server, one can use {!add_path_handler} and {!set_top_handler}
    to specify how to handle incoming requests.

    @param buf_size size for buffers (since 0.11)
    @param head_middlewares see {!add_head_middleware} for details (since 0.18)
    @param middlewares see {!add_middleware} for more details.
    @param enable_logging
      if true and [Logs] is installed, emit logs via Logs (since 0.18). Default
      [true].

    @since 0.14 *)

val addr : t -> string
(** Address on which the server listens. *)

val is_ipv6 : t -> bool
(** [is_ipv6 server] returns [true] iff the address of the server is an IPv6
    address.
    @since 0.3 *)

val port : t -> int
(** Port on which the server listens. Note that this might be different than the
    port initially given if the port was [0] (meaning that the OS picks a port
    for us). *)

val active_connections : t -> int
(** Number of currently active connections. *)

val add_decode_request_cb :
  t ->
  (unit Request.t -> (unit Request.t * (IO.Input.t -> IO.Input.t)) option) ->
  unit
[@@deprecated "use add_middleware"]
(** Add a callback for every request. The callback can provide a stream
    transformer and a new request (with modified headers, typically). A possible
    use is to handle decompression by looking for a [Transfer-Encoding] header
    and returning a stream transformer that decompresses on the fly.

    @deprecated use {!add_middleware} instead *)

val add_encode_response_cb :
  t -> (unit Request.t -> Response.t -> Response.t option) -> unit
[@@deprecated "use add_middleware"]
(** Add a callback for every request/response pair. Similarly to
    {!add_encode_response_cb} the callback can return a new response, for
    example to compress it. The callback is given the query with only its
    headers, as well as the current response.

    @deprecated use {!add_middleware} instead *)

val add_middleware :
  stage:[ `Encoding | `Stage of int ] -> t -> Middleware.t -> unit
(** Add a middleware to every request/response pair.
    @param stage
      specify when middleware applies. Encoding comes first (outermost layer),
      then stages in increasing order.
    @raise Invalid_argument if stage is [`Stage n] where [n < 1]
    @since 0.11 *)

val add_head_middleware : t -> Head_middleware.t -> unit
(** Add a request-header only {!Head_middleware.t}. This is called on requests,
    to modify them, and returns a new request immediately.
    @since 0.18 *)

(** {2 Request handlers} *)

val set_top_handler : t -> (IO.Input.t Request.t -> Response.t) -> unit
(** Setup a handler called by default.

    This handler is called with any request not accepted by any handler
    installed via {!add_path_handler}. If no top handler is installed, unhandled
    paths will return a [404] not found

    This used to take a [string Request.t] but it now takes a
    [byte_stream Request.t] since 0.14 . Use {!Request.read_body_full} to read
    the body into a string if needed. *)

val add_route_handler :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?middlewares:Middleware.t list ->
  ?meth:Meth.t ->
  t ->
  ('a, string Request.t -> Response.t) Route.t ->
  'a ->
  unit
(** [add_route_handler server Route.(exact "path" @/ string @/ int @/ return) f]
    calls [f "foo" 42 request] when a [request] with path "path/foo/42/" is
    received.

    Note that the handlers are called in the reverse order of their addition, so
    the last registered handler can override previously registered ones.

    @param meth
      if provided, only accept requests with the given method. Typically one
      could react to [`GET] or [`PUT].
    @param accept
      should return [Ok()] if the given request (before its body is read) should
      be accepted, [Error (code,message)] if it's to be rejected (e.g. because
      its content is too big, or for some permission error). See the
      {!http_of_dir} program for an example of how to use [accept] to filter
      uploads that are too large before the upload even starts. The default
      always returns [Ok()], i.e. it accepts all requests.

    @since 0.6 *)

val add_route_handler_stream :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?middlewares:Middleware.t list ->
  ?meth:Meth.t ->
  t ->
  ('a, IO.Input.t Request.t -> Response.t) Route.t ->
  'a ->
  unit
(** Similar to {!add_route_handler}, but where the body of the request is a
    stream of bytes that has not been read yet. This is useful when one wants to
    stream the body directly into a parser, json decoder (such as [Jsonm]) or
    into a file.
    @since 0.6 *)

(** {2 Server-sent events}

    {b EXPERIMENTAL}: this API is not stable yet. *)

(** A server-side function to generate of Server-sent events.

    See
    {{:https://html.spec.whatwg.org/multipage/server-sent-events.html} the w3c
     page} and
    {{:https://jvns.ca/blog/2021/01/12/day-36--server-sent-events-are-cool--and-a-fun-bug/}
     this blog post}.

    @since 0.9 *)
module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit
  (** Set headers of the response. This is not mandatory but if used at all, it
      must be called before any call to {!send_event} (once events are sent the
      response is already sent too). *)

  val send_event :
    ?event:string -> ?id:string -> ?retry:string -> data:string -> unit -> unit
  (** Send an event from the server. If data is a multiline string, it will be
      sent on separate "data:" lines. *)

  val close : unit -> unit
  (** Close connection.
      @since 0.11 *)
end

type server_sent_generator = (module SERVER_SENT_GENERATOR)
(** Server-sent event generator. This generates events that are forwarded to the
    client (e.g. the browser).
    @since 0.9 *)

val add_route_server_sent_handler :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?middlewares:Head_middleware.t list ->
  t ->
  ('a, string Request.t -> server_sent_generator -> unit) Route.t ->
  'a ->
  unit
(** Add a handler on an endpoint, that serves server-sent events.

    The callback is given a generator that can be used to send events as it
    pleases. The connection is always closed by the client, and the accepted
    method is always [GET]. This will set the header "content-type" to
    "text/event-stream" automatically and reply with a 200 immediately. See
    {!server_sent_generator} for more details.

    This handler stays on the original thread (it is synchronous).

    @since 0.9 *)

(** {2 Upgrade handlers}

    These handlers upgrade the connection to another protocol.
    @since 0.17 *)

(** Handler that upgrades to another protocol.
    @since 0.17 *)
module type UPGRADE_HANDLER = sig
  type handshake_state
  (** Some specific state returned after handshake *)

  val name : string
  (** Name in the "upgrade" header *)

  val handshake :
    Unix.sockaddr ->
    unit Request.t ->
    (Headers.t * handshake_state, string) result
  (** Perform the handshake and upgrade the connection. This returns either
      [Ok (resp_headers, state)] in case of success, in which case the server
      sends a [101] response with [resp_headers]; or it returns [Error log_msg]
      if the the handshake fails, in which case the connection is closed without
      further ado and [log_msg] is logged locally (but not returned to the
      client). *)

  val handle_connection : handshake_state -> IO.Input.t -> IO.Output.t -> unit
  (** Take control of the connection and take it from ther.e *)
end

type upgrade_handler = (module UPGRADE_HANDLER)
(** @since 0.17 *)

val add_upgrade_handler :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?middlewares:Head_middleware.t list ->
  t ->
  ('a, upgrade_handler) Route.t ->
  'a ->
  unit

(** {2 Run the server} *)

val running : t -> bool
(** Is the server running?
    @since 0.14 *)

val stop : t -> unit
(** Ask the server to stop. This might not have an immediate effect as {!run}
    might currently be waiting on IO. *)

val run : ?after_init:(unit -> unit) -> t -> (unit, exn) result
(** Run the main loop of the server, listening on a socket described at the
    server's creation time, using [new_thread] to start a thread for each new
    client.

    This returns [Ok ()] if the server exits gracefully, or [Error e] if it
    exits with an error.

    @param after_init is called after the server starts listening. since 0.13 .
*)

val run_exn : ?after_init:(unit -> unit) -> t -> unit
(** [run_exn s] is like [run s] but re-raises an exception if the server exits
    with an error.
    @since 0.14 *)
