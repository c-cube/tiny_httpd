(** HTTP Methods *)

type t = [ `GET | `PUT | `POST | `HEAD | `DELETE | `OPTIONS ]
(** A HTTP method.
      For now we only handle a subset of these.

      See https://tools.ietf.org/html/rfc7231#section-4 *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val of_string : string -> t
