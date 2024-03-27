(** Basic time measurement.

    This provides a basic clock, monotonic if [mtime] is installed,
    or based on [Unix.gettimeofday] otherwise *)

val now_us : unit -> float
(** Current time in microseconds. The precision should be at least below the millisecond. *)

val now_s : unit -> float
(** Current time in seconds. The precision should be at least below the millisecond. *)
