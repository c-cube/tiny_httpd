(** {1 Some utils for writing web servers}

    @since 0.2
*)

val percent_encode : ?skip:(char -> bool) -> string -> string
(** Encode the string into a valid path following
    https://tools.ietf.org/html/rfc3986#section-2.1
    @param skip if provided, allows to preserve some characters, e.g. '/' in a path.
*)

val percent_decode : string -> string option
(** Inverse operation of {!percent_encode}.
    Can fail since some strings are not valid percent encodings. *)

val split_query : string -> string * string
(** Split a path between the path and the query
    @since 0.5 *)

val split_on_slash : string -> string list
(** Split a string on ['/'], remove the trailing ['/'] if any.
    @since 0.6 *)

val get_non_query_path : string -> string
(** get the part of the path that is not the query parameters.
    @since 0.5 *)

val get_query : string -> string
(** Obtain the query part of a path.
    @since 0.4 *)

val parse_query : string -> ((string*string) list, string) result
(** Parse a query as a list of ['&'] or [';'] separated [key=value] pairs.
    The order might not be preserved.
    @since 0.3
*)
