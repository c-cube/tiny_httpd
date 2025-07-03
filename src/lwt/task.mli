(** Direct style tasks for Lwt *)

val run : (unit -> 'a) -> 'a Lwt.t
(** Run a microtask *)

val run_async : (unit -> unit) -> unit

val await : 'a Lwt.t -> 'a
(** Can only be used inside {!run} *)
