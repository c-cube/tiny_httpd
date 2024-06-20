(** Response Codes *)

type t = int
(** A standard HTTP code.

      https://tools.ietf.org/html/rfc7231#section-6 *)

val ok : t
(** The code [200] *)

val not_found : t
(** The code [404] *)

val descr : t -> string
(** A description of some of the error codes.
      NOTE: this is not complete (yet). *)

val is_success : t -> bool
(** [is_success code] is true iff [code] is in the [2xx] or [3xx] range.
      @since 0.17 *)
