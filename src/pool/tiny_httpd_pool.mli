
type t
(** A thread pool. *)

val create : ?j:int -> unit -> t

val run : t -> (unit -> unit) -> unit
(** [run pool f] schedules the task [f()] to be run in the pool
    when a worker thread becomes available. *)

val shutdown : t -> unit
