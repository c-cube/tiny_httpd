type stream = {
  is_fill_buf: 'a. (bytes -> int -> int -> 'a) -> 'a;
  is_consume: int -> unit;
  is_close: unit -> unit;
}
(** A buffer input stream, with a view into the current buffer (or refill if empty),
    and a function to consume [n] bytes *)

(** {2 Tiny buffer implementation} *)
module Buf_ : sig
  type t
  val size : t -> int
  val clear : t -> unit
  val create : ?size:int -> unit -> t
  val contents : t -> string
end

(** {2 Generic stream of data} *)
module Stream_ : sig
  type t = stream

  val close : t -> unit
  val of_chan : in_channel -> t
  val of_chan_close_noerr : in_channel -> t
  val of_bytes : ?i:int -> ?len:int -> bytes -> t
  val with_file : string -> (t -> 'a) -> 'a
  (** Open a file with given name, and obtain an input stream *)

  val read_line : ?buf:Buf_.t -> t -> string
  val read_all : ?buf:Buf_.t -> t -> string
end

module Meth : sig
  type t = [
    | `GET
    | `PUT
    | `POST
    | `HEAD
    | `DELETE
  ]

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

module Headers : sig
  type t = (string * string) list
  val get : ?f:(string->string) -> string -> t -> string option
  val set : string -> string -> t -> t
  val contains : string -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Request : sig
  type 'body t = {
    meth: Meth.t;
    headers: Headers.t;
    path: string;
    body: 'body;
  }

  val pp : Format.formatter -> string t -> unit
  val pp_ : Format.formatter -> _ t -> unit

  val headers : _ t -> Headers.t
  val get_header : ?f:(string->string) -> _ t -> string -> string option
  val get_header_int : _ t -> string -> int option
  val set_header : 'a t -> string -> string -> 'a t
  val meth : _ t -> Meth.t
  val path : _ t -> string
  val body : 'b t -> 'b
  val read_body_full : stream t -> string t
end

module Response_code : sig
  type t = int
  val ok : t
  val not_found : t
  val descr : t -> string
end

module Response : sig
  type body = [`String of string | `Stream of stream]
  type t

  val make_raw :
    ?headers:Headers.t ->
    code:Response_code.t ->
    string ->
    t

  val make_raw_stream :
    ?headers:Headers.t ->
    code:Response_code.t ->
    stream ->
    t

  val make :
    ?headers:Headers.t ->
    (body, Response_code.t * string) result -> t

  val make_string :
    ?headers:Headers.t ->
    (string, Response_code.t * string) result -> t

  val make_stream :
    ?headers:Headers.t ->
    (stream, Response_code.t * string) result -> t

  val fail : ?headers:Headers.t -> code:int ->
    ('a, unit, string, t) format4 -> 'a
  (** Make the current request fail with the given code and message.
      Example: [fail ~code:404 "oh noes, %s not found" "waldo"]
  *)

  val fail_raise : code:int -> ('a, unit, string, 'b) format4 -> 'a
  (** Similar to {!fail} but raises an exception that exits the current handler.
      This should not be used outside of a (path) handler.
      Example: [fail_raise ~code:404 "oh noes, %s not found" "waldo"; never_executed()]
  *)

  val pp : Format.formatter -> t -> unit
end

type t

val create :
  ?masksigpipe:bool ->
  ?new_thread:((unit -> unit) -> unit) ->
  ?addr:string ->
  ?port:int ->
  unit ->
  t
(** TODO: document *)

val addr : t -> string
val port : t -> int

val add_decode_request_cb :
  t ->
  (unit Request.t -> (unit Request.t * (stream -> stream)) option) -> unit
(** Add a callback for every request.
    The callback can provide a stream transformer and a new request (with
    modified headers, typically).
*)

val add_encode_response_cb:
  t -> (string Request.t -> Response.t -> Response.t option) -> unit
(** Add a callback for every request/response pair.
    Similarly to {!add_encode_response_cb} the callback can return a new
    response, for example to compress it.
    The callback is given the fully parsed query as well as the current
    response.
*)

val set_top_handler : t -> (string Request.t -> Response.t) -> unit
(** Setup a handler called by default.
    If not installed, unhandled paths will return a 404 not found. *)

val add_path_handler :
  ?accept:(unit Request.t -> (unit, Response_code.t * string) result) ->
  ?meth:Meth.t ->
  t ->
  ('a, Scanf.Scanning.in_channel,
   'b, 'c -> string Request.t -> Response.t, 'a -> 'd, 'd) format6 ->
  'c -> unit
(** [add_path_handler server "/some/path/%s@/%d/" f]
    calls [f request "foo" 42 ()] when a request with path "some/path/foo/42/"
    is received.
    This uses {!Scanf}'s splitting, which has some gotchas (in particular,
    ["%s"] is eager, so it's generally necessary to delimit its
    scope with a ["@/"] delimiter. The "@" before a character indicates it's
    a separator.
    @param meth if provided, only accept requests with the given method
    @param accept should return [true] if the given request (before its body
    is read) should be accepted, [false] if it's to be rejected (e.g. because
    its content is too big, or for some permission error).
*)

val stop : t -> unit
(** Ask the server to stop. This might not have an immediate effect
    as {!run} might currently be waiting on IO. *)

val run : t -> (unit, exn) result
(** Run the main loop of the server, listening on a socket
    described at the server's creation time, using [new_thread] to
    start a thread for each new client. *)

(**/**)

val _debug : ((('a, out_channel, unit, unit, unit, unit) format6 -> 'a) -> unit) -> unit
val _enable_debug: bool -> unit

(**/**)

