(** Expose metrics over HTTP in the prometheus format *)

type tags = (string * string) list

(** Registry for metrics. *)
module Registry : sig
  type t
  (** The registry contains a group of metrics *)

  val create : unit -> t

  val emit : Buffer.t -> t -> unit
  (** Write metrics into the given buffer. The buffer will be
      cleared first thing. *)

  val emit_str : t -> string
end

val global : Registry.t

(** Counters *)
module Counter : sig
  type t
  (** A counter, monotonically increasing *)

  val create : Registry.t -> ?tags:tags -> ?descr:string -> string -> t
  val incr : t -> unit
  val incr_by : t -> int -> unit
  val decr : t -> unit
  val decr_by : t -> int -> unit
end

(** Gauges *)
module Gauge : sig
  type t
  (** A gauge, taking arbitrary values *)

  val create : Registry.t -> ?tags:tags -> ?descr:string -> string -> t
  val set : t -> int -> unit
  val incr : t -> unit
  val incr_by : t -> int -> unit
  val decr : t -> unit
  val decr_by : t -> int -> unit
end

(* TODO:
   module Histogram : sig
   end
*)

val http_middleware : Registry.t -> Tiny_httpd.Middleware.t
(** Middleware to get basic metrics about HTTP requests *)

val add_route_to_server : Tiny_httpd.t -> Registry.t -> unit
(** Add a "/metrics" route to the server *)

val instrument_server : Tiny_httpd.t -> Registry.t -> unit
(** Add middleware and route *)
