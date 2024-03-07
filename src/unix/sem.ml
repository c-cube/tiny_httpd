(** semaphore, for limiting concurrency. *)

type t = { mutable n: int; max: int; mutex: Mutex.t; cond: Condition.t }

let create n =
  if n <= 0 then invalid_arg "Semaphore.create";
  { n; max = n; mutex = Mutex.create (); cond = Condition.create () }

let acquire m t =
  Mutex.lock t.mutex;
  while t.n < m do
    Condition.wait t.cond t.mutex
  done;
  assert (t.n >= m);
  t.n <- t.n - m;
  Condition.broadcast t.cond;
  Mutex.unlock t.mutex

let release m t =
  Mutex.lock t.mutex;
  t.n <- t.n + m;
  Condition.broadcast t.cond;
  Mutex.unlock t.mutex

let num_acquired t = t.max - t.n
