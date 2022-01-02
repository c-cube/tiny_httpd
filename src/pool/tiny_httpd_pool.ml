
(* atomics *)
module A = Tiny_httpd_atomic

(* guess how many cores we can use *)
let guess_cpu_count () =
  let default = 4 in
  try
    let cmd = "grep -c processor /proc/cpuinfo" in
    let p = Unix.open_process_in cmd in
    try
      let x = input_line p |> int_of_string in
      ignore (Unix.close_process_in p); x
    with _ -> ignore (Unix.close_process_in p); default
  with _ -> default


(* thread-safe queue.
   We mix "Implementing Lock-Free Queues", Valois 1994, with a parking lot
   for readers using a normal mutex + condition *)
module Q : sig
  type 'a t

  val create : dummy:'a -> unit -> 'a t

  val push : 'a t -> 'a -> unit
  (** Push an element. *)

  val pop : 'a t -> 'a
  (** pop the first element. Blocks if none is available. *)
end = struct
  type 'a node = {
    value: 'a;
    next: 'a node option A.t;
  }

  type 'a t = {
    head: 'a node A.t;
    tail: 'a node A.t;
    dummy: 'a;

    n_parked: int A.t; (* threads waiting *)
    park_lock: Mutex.t;
    park_cond: Condition.t;
  }

  let create ~dummy () : _ t =
    let ptr0 = {value=dummy;next=A.make None} in
    { head=A.make ptr0;
      tail=A.make ptr0;
      dummy;
      n_parked=A.make 0;
      park_lock=Mutex.create();
      park_cond=Condition.create();
    }

  let push (self:_ t) x : unit =
    (* new node to insert at the back *)
    let q = {value=x; next=A.make None} in

    let ok = ref false in
    while not !ok do
      let p = A.get self.tail in
      ok := A.compare_and_set p.next None (Some q);
      if not !ok then (
        (* try to ensure progress if another thread takes too long to update [tail] *)
        begin match A.get p.next with
          | None -> ()
          | Some p_next ->
            ignore (A.compare_and_set self.tail p p_next : bool)
        end;
      );
    done;

    (* if any thread is parked, try to unpark one thread *)
    if A.get self.n_parked > 0 then (
      Mutex.lock self.park_lock;
      Condition.signal self.park_cond;
      Mutex.unlock self.park_lock;
    )

  (* try to pop an element already in the queue *)
  let pop_nonblock self : _ option =
    let res = ref None in

    let continue = ref true in
    while !continue do
      let p = A.get self.head in
      match A.get p.next with
      | None ->
        continue := false; (* return None, queue is empty *)
      | Some p_next ->
        let ok = A.compare_and_set self.head p p_next in
        if ok then (
          res := Some p_next.value;
          continue := false;
        )
    done;
    !res

  let rec pop (self:'a t) : 'a =
    (* be on the safe side: assume we're going to park,
       so that if another thread pushes after the "PARK" line it'll unpark us *)
    A.incr self.n_parked;

    (* try to pop from queue *)
    begin match pop_nonblock self with
      | Some x ->
        A.decr self.n_parked;
        x
      | None ->
        (* PARK *)
        Mutex.lock self.park_lock;
        Condition.wait self.park_cond self.park_lock;
        Mutex.unlock self.park_lock;
        A.decr self.n_parked;
        (pop [@tailcall]) self
    end
end

type task = unit -> unit

type t = {
  tasks: task Q.t;
  threads: Thread.t array;
  active: bool A.t;
}

(* run a task in some background thread *)
let[@inline] run self (f:task) : unit =
  Q.push self.tasks f

exception Shutdown

let worker_ (tasks:task Q.t) : unit =
  let continue = ref true in
  while !continue do
    let f = Q.pop tasks in
    try f()
    with
    | Shutdown -> continue := false
    | e ->
      Printf.eprintf "tiny_httpd_pool: uncaught task exception:\n%s\n%!"
        (Printexc.to_string e)
  done

let max_threads_ = 256
let create ?(j=guess_cpu_count()) () : t =
  let j = min (max j 2) max_threads_ in
  Printf.eprintf "pool: %d threads\n%!" j;
  let tasks = Q.create ~dummy:(fun()->assert false) () in
  let threads = Array.init j (fun _ -> Thread.create worker_ tasks) in
  { tasks; threads; active=A.make true; }

let shutdown self =
  (* [if self.active then self.active <- false; …] *)
  if A.compare_and_set self.active true false then (
    for _i=1 to Array.length self.threads do
      run self (fun () -> raise Shutdown)
    done;
    Array.iter Thread.join self.threads
  )

