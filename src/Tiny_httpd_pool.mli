(** Resource pool *)

type 'a t
(** Pool of values of type ['a] *)

val create : mk_item:(unit -> 'a) -> ?max_size:int -> unit -> 'a t
(** Create a new pool. *)

val with_resource : 'a t -> ('a -> 'b) -> 'b
(** [with_resource pool f] runs [f x] with [x] a resource;
    when [f] fails or returns, [x] is returned to the pool for
    future reuse. *)
