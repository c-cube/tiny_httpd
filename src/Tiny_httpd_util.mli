(** {1 Some utils for writing web servers}

    @since NEXT_RELEASE
*)

val percent_encode : ?skip:(char -> bool) -> string -> string
(** Encode the string into a valid path following
    https://tools.ietf.org/html/rfc3986#section-2.1
    @param skip if provided, allows to preserve some characters, e.g. '/' in a path.
*)

val percent_decode : string -> string option
(** Inverse operation of {!percent_encode}.
    Can fail since some strings are not valid percent encodings. *)
