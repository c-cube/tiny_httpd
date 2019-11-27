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

val parse_query : string -> ((string*string) list, string) result
(** Parse a query as a list of ['&'] or [';'] separated [key=value] pairs.
    The order might not be preserved.
    @since 0.3
*)
