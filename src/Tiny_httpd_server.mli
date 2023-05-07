
(** HTTP server.

    This module implements a very simple, basic HTTP/1.1 server using blocking
    IOs and threads.

    It is possible to use a thread pool, see {!create}'s argument [new_thread].

    @since NEXT_RELEASE
*)

type buf = Tiny_httpd_buf.t
type byte_stream = Tiny_httpd_stream.t

(** {2 Methods} *)

module Meth : sig
  type t = [
    | `GET
    | `PUT
    | `POST
    | `HEAD
    | `DELETE
    | `OPTIONS
  ]
  (** A HTTP method.
      For now we only handle a subset of these.

      See https://tools.ietf.org/html/rfc7231#section-4 *)

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

(** {2 Headers}

    Headers are metadata associated with a request or response. *)

module Headers : sig
  type t = (string * string) list
  (** The header files of a request or response.

      Neither the key nor the value can contain ['\r'] or ['\n'].
      See https://tools.ietf.org/html/rfc7230#section-3.2 *)

  val empty : t
  (** Empty list of headers
      @since 0.5 *)

  val get : ?f:(string->string) -> string -> t -> string option
  (** [get k headers] looks for the header field with key [k].
      @param f if provided, will transform the value before it is returned. *)

  val set : string -> string -> t -> t
  (** [set k v headers] sets the key [k] to value [v].
      It erases any previous entry for [k] *)

  val remove : string -> t -> t
  (** Remove the key from the headers, if present. *)

  val contains : string -> t -> bool
  (** Is there a header with the given key? *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the headers. *)
end

(** {2 Requests}

    Requests are sent by a client, e.g. a web browser or cURL. *)

module Request : sig
  type 'body t = private {
    meth: Meth.t;
    host: string;
    headers: Headers.t;
    http_version: int*int;
    path: string;
    path_components: string list;
    query: (string*string) list;
    body: 'body;
    start_time: float;
    (** Obtained via [get_time_s] in {!create}
        @since 0.11 *)
  }
  (** A request with method, path, host, headers, and a body, sent by a client.

      The body is polymorphic because the request goes through
      several transformations. First it has no body, as only the request
      and headers are read; then it has a stream body; then the body might be
      entirely read as a string via {!read_body_full}.

      @since 0.6 The field [query] was added and contains the query parameters in ["?foo=bar,x=y"]
      @since 0.6 The field [path_components] is the part of the path that precedes [query] and is split on ["/"].
      @since 0.11 the type is a private alias
      @since 0.11 the field [start_time] was added
  *)

  val pp : Format.formatter -> string t -> unit
  (** Pretty print the request and its body *)

  val pp_ : Format.formatter -> _ t -> unit
  (** Pretty print the request without its body *)

  val headers : _ t -> Headers.t
  (** List of headers of the request, including ["Host"] *)

  val get_header : ?f:(string->string) -> _ t -> string -> string option

  val get_header_int : _ t -> string -> int option

  val set_header : string -> string -> 'a t -> 'a t
  (** [set_header k v req] sets [k: v] in the request [req]'s headers. *)

  val update_headers : (Headers.t -> Headers.t) -> 'a t -> 'a t
  (** Modify headers
      @since 0.11 *)

  val set_body : 'a -> _ t -> 'a t
  (** [set_body b req] returns a new query whose body is [b].
      @since 0.11 *)

  val host : _ t -> string
  (** Host field of the request. It also appears in the headers. *)

  val meth : _ t -> Meth.t
  (** Method for the request. *)

  val path : _ t -> string
  (** Request path. *)

  val query : _ t -> (string*string) list
  (** Decode the query part of the {!path} field
      @since 0.4 *)

  val body : 'b t -> 'b
  (** Request body, possibly empty. *)

  val start_time : _ t -> float
  (** time stamp (from {!Unix.gettimeofday}) after parsing the first line of the request
      @since 0.11 *)

  val limit_body_size : max_size:int -> byte_stream t -> byte_stream t
  (** Limit the body size to [max_size] bytes, or return
      a [413] error.
      @since 0.3
  *)

  val read_body_full : ?buf_size:int -> byte_stream t -> string t
  (** Read the whole body into a string. Potentially blocking.

      @param buf_size initial size of underlying buffer (since 0.11) *)

  (**/**)
  (* for testing purpose, do not use *)
  module Internal_ : sig
    val parse_req_start : ?buf:buf -> get_time_s:(unit -> float) -> byte_stream -> unit t option
    val parse_body : ?buf:buf -> unit t -> byte_stream -> byte_stream t
  end
  (**/**)
end

(** {2 Response Codes} *)

module Response_code : sig
  type t = int
  (** A standard HTTP code.

      https://tools.ietf.org/html/rfc7231#section-6 *)

  val ok : t
  (** The code [200] *)

  val not_found : t
  (** The code [404] *)

  val descr : t -> string
  (** A description of some of the error codes.
      NOTE: this is not complete (yet). *)
end

(** {2 Responses}

    Responses are what a http server, such as {!Tiny_httpd}, send back to
    the client to answer a {!Request.t}*)

module Response : sig
  type body = [`String of string | `Stream of byte_stream | `Void]
  (** Body of a response, either as a simple string,
      or a stream of bytes, or nothing (for server-sent events). *)

  type t = private {
    code: Response_code.t; (** HTTP response code. See {!Response_code}. *)
    headers: Headers.t; (** Headers of the reply. Some will be set by [Tiny_httpd] automatically. *)
    body: body; (** Body of the response. Can be empty. *)
  }
  (** A response to send back to a client. *)

  val set_body : body -> t -> t
  (** Set the body of the response.
      @since 0.11 *)

  val set_header : string -> string -> t -> t
  (** Set a header.
      @since 0.11 *)

  val update_headers : (Headers.t -> Headers.t) -> t -> t
  (** Modify headers
      @since 0.11 *)

  val set_headers : Headers.t -> t -> t
  (** Set all headers.
      @since 0.11 *)

  val set_code : Response_code.t -> t -> t
  (** Set the response code.
      @since 0.11 *)

  val make_raw :
    ?headers:Headers.t ->
    code:Response_code.t ->
    string ->
    t
  (** Make a response from its raw components, with a string body.
      Use [""] to not send a body at all. *)

  val make_raw_stream :
    ?headers:Headers.t ->
    code:Response_code.t ->
    byte_stream ->
    t
  (** Same as {!make_raw} but with a stream body. The body will be sent with
      the chunked transfer-encoding. *)

  val make :
    ?headers:Headers.t ->
    (body, Response_code.t * string) result -> t
  (** [make r] turns a result into a response.

      - [make (Ok body)] replies with [200] and the body.
      - [make (Error (code,msg))] replies with the given error code
        and message as body.
  *)

  val make_string :
    ?headers:Headers.t ->
    (string, Response_code.t * string) result -> t
  (** Same as {!make} but with a string body. *)

  val make_stream :
    ?headers:Headers.t ->
    (byte_stream, Response_code.t * string) result -> t
  (** Same as {!make} but with a stream body. *)

  val fail : ?headers:Headers.t -> code:int ->
    ('a, unit, string, t) format4 -> 'a
  (** Make the current request fail with the given code and message.
      Example: [fail ~code:404 "oh noes, %s not found" "waldo"].
  *)

  val fail_raise : code:int -> ('a, unit, string, 'b) format4 -> 'a
  (** Similar to {!fail} but raises an exception that exits the current handler.
      This should not be used outside of a (path) handler.
      Example: [fail_raise ~code:404 "oh noes, %s not found" "waldo"; never_executed()]
  *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the response. *)
end

(** {2 Routing}

    Basic type-safe routing.
    @since 0.6 *)
module Route : sig
  type ('a, 'b) comp
  (** An atomic component of a path *)

  type ('a, 'b) t
  (** A route, composed of path components *)

  val int : (int -> 'a, 'a) comp
  (** Matches an integer. *)

  val string : (string -> 'a, 'a) comp
  (** Matches a string not containing ['/'] and binds it as is. *)

  val string_urlencoded : (string -> 'a, 'a) comp
  (** Matches a URL-encoded string, and decodes it. *)

  val exact : string -> ('a, 'a) comp
  (** [exact "s"] matches ["s"] and nothing else. *)

  val return : ('a, 'a) t
  (** Matches the empty path. *)

  val rest_of_path : (string -> 'a, 'a) t
  (** Matches a string, even containing ['/']. This will match
      the entirety of the remaining route.
      @since 0.7 *)

  val rest_of_path_urlencoded : (string -> 'a, 'a) t
  (** Matches a string, even containing ['/'], an URL-decode it.
      This will match the entirety of the remaining route.
      @since 0.7 *)

  val (@/) : ('a, 'b) comp -> ('b, 'c) t -> ('a, 'c) t
  (** [comp / route] matches ["foo/bar/…"] iff [comp] matches ["foo"],
      and [route] matches ["bar/…"]. *)

  val exact_path : string -> ('a,'b) t -> ('a,'b) t
  (** [exact_path "foo/bar/..." r] is equivalent to
      [exact "foo" @/ exact "bar" @/ ... @/ r]
      @since 0.11 **)

  val pp : Format.formatter -> _ t -> unit
  (** Print the route.
      @since 0.7 *)

  val to_string : _ t -> string
  (** Print the route.
      @since 0.7 *)
end

(** {2 Middlewares}

    A middleware can be inserted in a handler to modify or observe
    its behavior.

    @since 0.11
*)
module Middleware : sig
  type handler = byte_stream Request.t -> resp:(Response.t -> unit) -> unit
  (** Handlers are functions returning a response to a request.
      The response can be delayed, hence the use of a continuation
      as the [resp] parameter. *)

  type t = handler -> handler
  (** A middleware is a handler transformation.

      It takes the existing handler [h],
      and returns a new one which, given a query, modify it or log it
      before passing it to [h], or fail. It can also log or modify or drop
      the response. *)

  val nil : t
  (** Trivial middleware that does nothing. *)
end

(** {2 Main Server type} *)

type t
(** A HTTP server. See {!create} for more details. *)

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
  ?middlewares:([`Encoding | `Stage of int] * Middleware.t) list ->
  unit ->
  t
(** Create a new webserver.

    The server will not do anything until {!run} is called on it.
    Before starting the server, one can use {!add_path_handler} and
    {!set_top_handler} to specify how to handle incoming requests.

    @param masksigpipe if true, block the signal {!Sys.sigpipe} which otherwise
    tends to kill client threads when they try to write on broken sockets. Default: [true].

    @param buf_size size for buffers (since 0.11)

    @param new_thread a function used to spawn a new thread to handle a
    new client connection. By default it is {!Thread.create} but one
    could use a thread pool instead.

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

val addr : t -> string
(** Address on which the server listens. *)

val is_ipv6 : t -> bool
(** [is_ipv6 server] returns [true] iff the address of the server is an IPv6 address.
    @since 0.3 *)

val port : t -> int
(** Port on which the server listens. *)

val active_connections : t -> int
(** Number of active connections *)

val add_decode_request_cb :
  t ->
  (unit Request.t -> (unit Request.t * (byte_stream -> byte_stream)) option) -> unit
[@@deprecated "use add_middleware"]
(** Add a callback for every request.
    The callback can provide a stream transformer and a new request (with
    modified headers, typically).
    A possible use is to handle decompression by looking for a [Transfer-Encoding]
    header and returning a stream transformer that decompresses on the fly.

    @deprecated use {!add_middleware} instead
*)

val add_encode_response_cb:
  t -> (unit Request.t -> Response.t -> Response.t option) -> unit
[@@deprecated "use add_middleware"]
(** Add a callback for every request/response pair.
    Similarly to {!add_encode_response_cb} the callback can return a new
    response, for example to compress it.
    The callback is given the query with only its headers,
    as well as the current response.

    @deprecated use {!add_middleware} instead
*)

val add_middleware :
  stage:[`Encoding | `Stage of int] ->
  t -> Middleware.t -> unit
(** Add a middleware to every request/response pair.
    @param stage specify when middleware applies.
      Encoding comes first (outermost layer), then stages in increasing order.
    @raise Invalid_argument if stage is [`Stage n] where [n < 1]
    @since 0.11
*)

(** {2 Request handlers} *)

val set_top_handler : t -> (string Request.t -> Response.t) -> unit
(** Setup a handler called by default.

    This handler is called with any request not accepted by any handler
    installed via {!add_path_handler}.
    If no top handler is installed, unhandled paths will return a [404] not found. *)

val add_route_handler :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?middlewares:Middleware.t list ->
  ?meth:Meth.t ->
  t ->
  ('a, string Request.t -> Response.t) Route.t -> 'a ->
  unit
(** [add_route_handler server Route.(exact "path" @/ string @/ int @/ return) f]
    calls [f "foo" 42 request] when a [request] with path "path/foo/42/"
    is received.

    Note that the handlers are called in the reverse order of their addition,
    so the last registered handler can override previously registered ones.

    @param meth if provided, only accept requests with the given method.
    Typically one could react to [`GET] or [`PUT].
    @param accept should return [Ok()] if the given request (before its body
    is read) should be accepted, [Error (code,message)] if it's to be rejected (e.g. because
    its content is too big, or for some permission error).
    See the {!http_of_dir} program for an example of how to use [accept] to
    filter uploads that are too large before the upload even starts.
    The default always returns [Ok()], i.e. it accepts all requests.

    @since 0.6
*)

val add_route_handler_stream :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?middlewares:Middleware.t list ->
  ?meth:Meth.t ->
  t ->
  ('a, byte_stream Request.t -> Response.t) Route.t -> 'a ->
  unit
(** Similar to {!add_route_handler}, but where the body of the request
    is a stream of bytes that has not been read yet.
    This is useful when one wants to stream the body directly into a parser,
    json decoder (such as [Jsonm]) or into a file.
    @since 0.6 *)

(** {2 Server-sent events}

    {b EXPERIMENTAL}: this API is not stable yet. *)

(** A server-side function to generate of Server-sent events.

    See {{: https://html.spec.whatwg.org/multipage/server-sent-events.html} the w3c page}
    and {{: https://jvns.ca/blog/2021/01/12/day-36--server-sent-events-are-cool--and-a-fun-bug/}
    this blog post}.

    @since 0.9
  *)
module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit
  (** Set headers of the response.
      This is not mandatory but if used at all, it must be called before
      any call to {!send_event} (once events are sent the response is
      already sent too). *)

  val send_event :
    ?event:string ->
    ?id:string ->
    ?retry:string ->
    data:string ->
    unit -> unit
  (** Send an event from the server.
      If data is a multiline string, it will be sent on separate "data:" lines. *)

  val close : unit -> unit
  (** Close connection.
      @since 0.11 *)
end

type server_sent_generator = (module SERVER_SENT_GENERATOR)
(** Server-sent event generator
    @since 0.9 *)

val add_route_server_sent_handler :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  t ->
  ('a, string Request.t -> server_sent_generator -> unit) Route.t -> 'a ->
  unit
(** Add a handler on an endpoint, that serves server-sent events.

    The callback is given a generator that can be used to send events
    as it pleases. The connection is always closed by the client,
    and the accepted method is always [GET].
    This will set the header "content-type" to "text/event-stream" automatically
    and reply with a 200 immediately.
    See {!server_sent_generator} for more details.

    This handler stays on the original thread (it is synchronous).

    @since 0.9 *)

(** {2 Run the server} *)

val stop : t -> unit
(** Ask the server to stop. This might not have an immediate effect
    as {!run} might currently be waiting on IO. *)

val run : t -> (unit, exn) result
(** Run the main loop of the server, listening on a socket
    described at the server's creation time, using [new_thread] to
    start a thread for each new client.

    This returns [Ok ()] if the server exits gracefully, or [Error e] if
    it exits with an error. *)

(**/**)

val _debug : ((('a, out_channel, unit, unit, unit, unit) format6 -> 'a) -> unit) -> unit
val _enable_debug: bool -> unit

(**/**)
