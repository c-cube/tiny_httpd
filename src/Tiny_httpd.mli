
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
end

module Response_code : sig
  type t = int

  val descr : t -> string
end

module Response : sig
  type out_stream = bytes -> int -> int -> int
  type t

  val make_raw :
    ?headers:Headers.t ->
    code:Response_code.t ->
    string ->
    t

  val make_raw_chunked :
    ?headers:Headers.t ->
    code:Response_code.t ->
    out_stream ->
    t

  val make :
    ?headers:Headers.t ->
    (string, Response_code.t * string) result -> t

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
  ?fork:((unit -> unit) -> unit) ->
  ?addr:string ->
  ?port:int ->
  unit ->
  t

val addr : t -> string
val port : t -> int

val add_decode_request_cb : t -> (string Request.t -> string Request.t option) -> unit
(** Add a callback for every request.
    The callback can modify the request by returning [Some r'] where [r']
    is the new request, or just perform side effects (logging?) and return [None].
*)

val add_encode_response_cb: t -> (string Request.t -> Response.t -> Response.t option) -> unit
(** Add a callback for every request/response pair.
    Similarly to {!add_encode_response_cb} the callback can return a new
    response, for example to compress it. *)

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
val run : t -> (unit, exn) result


(**/**)

val _debug : ((('a, out_channel, unit, unit, unit, unit) format6 -> 'a) -> unit) -> unit
val _enable_debug: bool -> unit

(**/**)

