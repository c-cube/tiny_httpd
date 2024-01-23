(** Logging for tiny_httpd *)

val info : ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
val debug : ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
val error : ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit

val setup : debug:bool -> unit -> unit
(** Setup and enable logging. This should only ever be used in executables,
    not libraries.
    @param debug if true, set logging to debug (otherwise info) *)
