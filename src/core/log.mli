(** Logging for tiny_httpd *)

val info : ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
val debug : ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit
val error : ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) -> unit

val setup : debug:bool -> unit -> unit
(** Setup and enable logging. This should only ever be used in executables, not
    libraries.
    @param debug if true, set logging to debug (otherwise info) *)

val dummy : bool

val fully_disable : unit -> unit
(** Totally silence logs for tiny_httpd. With [Logs] installed this means
    setting the level of the tiny_httpd source to [None].
    @since 0.18 *)
