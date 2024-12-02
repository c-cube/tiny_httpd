(** Headers

    Headers are metadata associated with a request or response. *)

type t = (string * string) list
(** The header files of a request or response.

      Neither the key nor the value can contain ['\r'] or ['\n'].
      See https://tools.ietf.org/html/rfc7230#section-3.2 *)

val empty : t
(** Empty list of headers.
      @since 0.5 *)

val get : ?f:(string -> string) -> string -> t -> string option
(** [get k headers] looks for the header field with key [k].
      @param f if provided, will transform the value before it is returned. *)

val get_exn : ?f:(string -> string) -> string -> t -> string
(** @raise Not_found *)

val set : string -> string -> t -> t
(** [set k v headers] sets the key [k] to value [v].
      It erases any previous entry for [k] *)

val remove : string -> t -> t
(** Remove the key from the headers, if present. *)

val contains : string -> t -> bool
(** Is there a header with the given key? *)

val pp : Format.formatter -> t -> unit
(** Pretty print the headers. *)

val parse_ : buf:Buf.t -> IO.Input.t -> t
(**/*)

val parse_line_ : string -> (string * string, string) result
(**/*)
