
(** Simple buffer.

    These buffers are used to avoid allocating too many byte arrays when
    processing streams and parsing requests.

    @since 0.12
*)

type t
val size : t -> int
val clear : t -> unit
val create : ?size:int -> unit -> t
val contents : t -> string

val bytes_slice : t -> bytes
(** Access underlying slice of bytes.
    @since 0.5 *)

val contents_and_clear : t -> string
(** Get contents of the buffer and clear it.
    @since 0.5 *)

val add_bytes : t -> bytes -> int -> int -> unit
(** Append given bytes slice to the buffer.
    @since 0.5 *)

