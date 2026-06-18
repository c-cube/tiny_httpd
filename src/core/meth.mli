(** HTTP Methods *)

type t = [ `GET | `PUT | `POST | `HEAD | `DELETE | `OPTIONS | `QUERY ]
(** A HTTP method. For now we only handle a subset of these.

    See https://tools.ietf.org/html/rfc7231#section-4 

    [`QUERY] added @since NEXT_RELEASE , see https://www.rfc-editor.org/info/rfc10008/
  and https://blainsmith.com/articles/rfc-10008-http-query-method/
    *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val of_string : string -> t
