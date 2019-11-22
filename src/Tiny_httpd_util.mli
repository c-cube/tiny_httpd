(** {1 Some utils for writing web servers}

    @since NEXT_RELEASE
*)

val percent_encode : string -> string
(** Encode the string into a valid path following
    https://tools.ietf.org/html/rfc3986#section-2.1
*)

val percent_decode : string -> string option
(** Inverse operation of {!percent_encode}.
    Can fail since some strings are not valid percent encodings. *)
