
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
  val pp : Format.formatter -> t -> unit
end

module Request : sig
  type t

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
  type t

  val make :
    ?headers:Headers.t ->
    code:Response_code.t ->
    string ->
    t

  val make_ok : ?headers:Headers.t -> string -> t
  val make_not_found : ?headers:Headers.t -> string -> t
  val make_error : ?headers:Headers.t -> string -> t

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


