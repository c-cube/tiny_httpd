type input_stream = (bytes -> int -> int -> int) * (unit -> unit)
(** An input stream is a function to read bytes into a buffer,
    and a function to close *)

type output_stream = (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit)
(** An output stream is a function to output bytes, a function to [flush],
    and a function to close. *)

(** {2 Tiny buffer implementation} *)
module Buf_ : sig
  type t
  val clear : t -> unit
  val create : ?size:int -> unit -> t
  val contents : t -> string
end

(** {2 Generic input stream} *)
module Input_stream : sig
  type t = input_stream

  val of_chan : in_channel -> t
  val of_chan_close_noerr : in_channel -> t
  val of_string : ?i:int -> ?len:int -> string -> t
  val of_bytes : ?i:int -> ?len:int -> bytes -> t
  val close : t -> unit
  val with_file : string -> (t -> 'a) -> 'a
  (** Open a file with given name, and obtain an input stream *)

  val read_line : ?buf:Buf_.t -> t -> string
  val read_all : ?buf:Buf_.t -> t -> string
end

(** {2 Generic output stream} *)
module Output_stream : sig
  type t = output_stream
  val of_chan : out_channel -> t
  val of_chan_close_noerr : out_channel -> t
  val of_buf : Buf_.t -> t
  val write : t -> string -> unit
  val flush : t -> unit
  val close : t -> unit

  val with_file : string -> (t -> 'a) -> 'a
  (** Open a file with given name, and obtain an output stream *)
end

val pipe : ?buf:Buf_.t -> input_stream -> output_stream -> unit
(** [pipe is os] pipes the content of [is] into [os]. *)

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
  val get : string -> t -> string option
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

  val headers : _ t -> Headers.t
  val get_header : _ t -> string -> string option
  val get_header_int : _ t -> string -> int option
  val meth : _ t -> Meth.t
  val path : _ t -> string
  val body : 'b t -> 'b
  val read_body_full : ?buf:Buf_.t -> input_stream t -> string t
end

module Response_code : sig
  type t = int
  val ok : t
  val not_found : t
  val descr : t -> string
end

module Response : sig
  type body = [`String of string | `Stream of input_stream]
  type t

  val make_raw :
    ?headers:Headers.t ->
    code:Response_code.t ->
    string ->
    t

  val make_raw_stream :
    ?headers:Headers.t ->
    code:Response_code.t ->
    input_stream ->
    t

  val make :
    ?headers:Headers.t ->
    (body, Response_code.t * string) result -> t

  val make_string :
    ?headers:Headers.t ->
    (string, Response_code.t * string) result -> t

  val make_stream :
    ?headers:Headers.t ->
    (input_stream, Response_code.t * string) result -> t

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
  (input_stream Request.t -> input_stream Request.t option) -> unit
(** Add a callback for every request.
    The callback can modify the request by returning [Some r'] where [r']
    is the new request, or just perform side effects (logging?) and return [None].
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

