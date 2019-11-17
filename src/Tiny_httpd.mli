
module Meth : sig
  type t = [
    | `GET
    | `PUT
    | `POST
    | `HEAD
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
  type t = {
    meth: Meth.t;
    headers: Headers.t;
    path: string;
    body: string
  }

  val pp : Format.formatter -> t -> unit

  val headers : t -> Headers.t
  val meth : t -> Meth.t
  val path : t -> string
  val body : t -> string
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

val add_request_cb : t -> (Request.t -> Request.t option) -> unit
(** Add a callback for every request.
    The callback can modify the request by returning [Some r'] where [r']
    is the new request, or just perform side effects (logging?) and return [None].
*)

val add_response_cb : t -> (Request.t -> Response.t -> Response.t option) -> unit
(** Add a callback for every request/response pair.
    Similarly to {!add_request_cb} the callback can modify the response. *)

val set_top_handler : t -> (Request.t -> Response.t) -> unit
(** Setup a handler called by default.
    If not installed, unhandled paths will return a 404 not found. *)

val add_path_handler :
  ?meth:Meth.t ->
  t ->
  ('a, Scanf.Scanning.in_channel, 'b, 'c -> unit -> Response.t, 'a -> 'd, 'd) format6 ->
  (Request.t -> 'c) -> unit
(** [add_path_handler server "/some/path/%s@/%d/" f]
    calls [f request "foo" 42 ()] when a request with path "some/path/foo/42/"
    is received.
    This uses {!Scanf}'s splitting, which has some gotchas (in particular,
    ["%s"] is eager, so it's generally necessary to delimit its
    scope with a ["@/"] delimiter. The "@" before a character indicates it's
    a separator.
    @param meth if provided, only accept requests with the given method
*)

val stop : t -> unit
val run : t -> (unit, exn) result


(**/**)

val _debug : ((('a, out_channel, unit, unit, unit, unit) format6 -> 'a) -> unit) -> unit

(**/**)

