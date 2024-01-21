(** Expose metrics over HTTP in the prometheus format *)

type tags = (string * string) list

(** Registry for metrics. *)
module Registry : sig
  type t
  (** The registry contains a group of metrics *)

  val create : unit -> t

  val on_will_emit : t -> (unit -> unit) -> unit
  (** [on_will_emit registry f] calls [f()] every time
      [emit buf registry] is called (before the metrics start being emitted). This
      is useful to update some metrics on demand. *)

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

  val incr_to : t -> int -> unit
  (** Increment to the given number. If it's lower than the current
      value this does nothing *)
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

module Histogram : sig
  type t
  (** Histogram *)

  val create :
    Registry.t ->
    ?tags:tags ->
    ?descr:string ->
    buckets:float list ->
    string ->
    t

  val add : t -> float -> unit
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

module GC_metrics : sig
  type t

  val create : Registry.t -> t
  val update : t -> unit

  val create_and_update_before_emit : Registry.t -> unit
  (** [create_and_update_before_emit reg] creates new GC metrics,
    adds them to the registry, and uses {!Registry.on_will_emit}
    to {!update} the metrics every time the registry is polled. *)
end
