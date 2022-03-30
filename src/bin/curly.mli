module Meth : sig
  type t =
    [ `GET
    | `POST
    | `HEAD
    | `PUT
    | `DELETE
    | `OPTIONS
    | `TRACE
    | `CONNECT
    | `PATCH
    | `Other of string ]

  val pp : Format.formatter -> t -> unit
end

module Header : sig
  type t = (string * string) list

  val pp : Format.formatter -> t -> unit
end

module Response : sig
  type t =
    { code: int
    ; headers: Header.t
    ; body:string
    }

  val pp : Format.formatter -> t -> unit
end

module Request : sig
  type t =
    { meth: Meth.t
    ; url:string
    ; headers: Header.t
    ; body:string
    }

  val make
    : ?headers:Header.t
    -> ?body:string
    -> url:string
    -> meth:Meth.t
    -> unit
    -> t

  val to_cmd_args : t -> string list

  val pp : Format.formatter -> t -> unit
end

module Process_result : sig
  type t =
    { status: Unix.process_status
    ; stderr:string
    ; stdout:string
    }

  val pp : Format.formatter -> t -> unit
end

module Error : sig
  type t =
    | Invalid_request of string
    | Bad_exit of Process_result.t
    | Failed_to_read_response of exn * Process_result.t
    | Exn of exn

  val pp : Format.formatter -> t -> unit
end

val run
  : ?exe:string
  -> ?args:string list
  -> Request.t
  -> (Response.t, Error.t) Result.result

val get
  : ?exe:string
  -> ?args:string list
  -> ?headers:Header.t
  -> string
  -> (Response.t, Error.t) Result.result
(** Specialized version of {!run} for method [`GET]
    @since 0.2.0 *)

val head
  : ?exe:string
  -> ?args:string list
  -> ?headers:Header.t
  -> string
  -> (Response.t, Error.t) Result.result
(** Specialized version of {!run} for method [`HEAD]
    @since 0.2.0 *)

val delete
  : ?exe:string
  -> ?args:string list
  -> ?headers:Header.t
  -> string
  -> (Response.t, Error.t) Result.result
(** Specialized version of {!run} for method [`DELETE]
    @since 0.2.0 *)

val post 
  : ?exe:string
  -> ?args:string list
  -> ?headers:Header.t
  -> ?body:string
  -> string
  -> (Response.t, Error.t) Result.result
(** Specialized version of {!run} for method [`POST]
    @since 0.2.0 *)

val put
  : ?exe:string
  -> ?args:string list
  -> ?headers:Header.t
  -> ?body:string
  -> string
  -> (Response.t, Error.t) Result.result
(** Specialized version of {!run} for method [`PUT]
    @since 0.2.0 *)
