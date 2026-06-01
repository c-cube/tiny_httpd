[API reference](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html)

# **domain-local-await** &mdash; Scheduler independent blocking

A low level mechanism intended for writing higher level libraries that need to
block in a scheduler friendly manner.

A library that needs to suspend and later resume the current thread of execution
may simply call
[`prepare_for_await`](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html#val-prepare_for_await)
to obtain a pair of
[`await`](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html#type-t.await)
and
[`release`](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html#type-t.release)
operations for the purpose.

To provide an efficient and scheduler friendly implementation of the mechanism,
schedulers may install an implementation by wrapping the scheduler main loop
with a call to
[`using`](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html#val-using).
The implementation is then stored in a domain, and optionally thread, local
variable. The overhead that this imposes on a scheduler should be insignificant.

An application can then choose to use schedulers that provide the necessary
implementation. An implementation that works with plain domains and threads is
provided as a default.

The end result is effective interoperability between schedulers and concurrent
programming libraries.

## Contents

- [Example: Concurrency-safe lazy](#example-concurrency-safe-lazy)
- [Example: Scheduler-friendly Mutex](#example-scheduler-friendly-mutex)
- [Example: Awaitable atomic locations](#example-awaitable-atomic-locations)
- [Example: Transparently asynchronous IO](#example-transparently-asynchronous-io)
- [References](#references)

## Example: Concurrency-safe lazy

At the time of writing this, the documentation of the Stdlib `Lazy` module
includes the following note:

> Note: `Lazy.force` is not concurrency-safe. If you use this module with
> multiple fibers, systhreads or domains, then you will need to add some locks.

Let's build a draft of a concurrency-safe version of lazy using atomics and
domain-local-await!

First we need to require the library:

<!--
```ocaml
# #thread
# #require "domain_shims"
```
-->

```ocaml
# #require "domain-local-await"
```

Here is a pair of types to represent the internal state of a lazy computation:

```ocaml
type 'a state =
  | Fun of (unit -> 'a)
  | Run of (unit -> unit) list
  | Val of 'a
  | Exn of exn

type 'a lazy_t = 'a state Atomic.t
```

A lazy computation starts as a thunk:

```ocaml
# let from_fun th = Atomic.make (Fun th)
val from_fun : (unit -> 'a) -> 'a state Atomic.t = <fun>
```

Or can be directly constructed with the given value:

```ocaml
# let from_val v = Atomic.make (Val v)
val from_val : 'a -> 'a state Atomic.t = <fun>
```

The interesting bits are in the `force` implementation:

```ocaml
# let rec force t =
    match Atomic.get t with
    | Val v -> v
    | Exn e -> raise e
    | Fun th as before ->
      if Atomic.compare_and_set t before (Run []) then
        let result =
          match th () with
          | v -> Val v
          | exception e -> Exn e
        in
        match Atomic.exchange t result with
        | (Val _ | Exn _ | Fun _) ->
          failwith "impossible"
        | Run waiters ->
          List.iter ((|>) ()) waiters;
          force t
      else
        force t
    | Run waiters as before ->
      let dla = Domain_local_await.prepare_for_await () in
      let after = Run (dla.release :: waiters) in
      if Atomic.compare_and_set t before after then
        match dla.await () with
        | () ->
          force t
        | exception cancelation_exn ->
          let rec cleanup () =
            match Atomic.get t with
            | (Val _ | Exn _ | Fun _) ->
              ()
            | Run waiters as before ->
              let after = Run (List.filter ((!=) dla.release) waiters) in
              if not (Atomic.compare_and_set t before after) then
                cleanup ()
          in
          cleanup ();
          raise cancelation_exn
      else
        force t
val force : 'a state Atomic.t -> 'a = <fun>
```

First `force` examines the state of the lazy computation. In case the result is
already known, the value is returned or the exception is raised. Otherwise
either the computation is started or the current thread of execution is
suspended using domain-local-await. Once the thunk returns, the lazy is updated
with the new state, any awaiters are released, and then all the `force` attempts
will retry to examine the result. Notice also that the above `force`
implementation is careful to perform a `cleanup` in case the `await` call raises
an exception, which indicates cancellation.

Let's then try it by creating a lazy computation and forcing it from two
different domains:

```ocaml
# let hello =
    from_fun (fun () ->
    Unix.sleepf 0.25;
    "Hello!")
val hello : string state Atomic.t = <abstr>

# let other = Domain.spawn (fun () -> force hello)
val other : string Domain.t = <abstr>

# force hello
- : string = "Hello!"

# Domain.join other
- : string = "Hello!"
```

Hello, indeed!

Note that the above implementation of lazy is intentionally kept relatively
simple. It could be optimized slightly to reduce allocations and proper
propagation of exception backtraces should be implemented. It could also be
useful to have a scheduler independent mechanism to get a unique id
corresponding to the current fiber, systhread, or domain and store that in the
lazy state to be able to give an error in case of recursive forcing.

## Example: Scheduler-friendly Mutex

At the time of writing this, the Stdlib `Mutex` implementation does not take
into account the possibility of having an effects based scheduler and simply
blocks the current domain (or (sys)thread) without giving a potential scheduler
the opportunity to schedule another fiber on the domain.

Let's build a draft of a scheduler-friendly mutex using atomics and
domain-local-await.

Here is a pair of types to represent a mutex:

```ocaml
type state =
  | Unlocked
  | Locked of (unit -> unit) list

type mutex = state Atomic.t
```

Essentially, a mutex is either unlocked or locked with a list of awaiters.

To construct a mutex we simply allocate a new atomic:

```ocaml
# let mutex () = Atomic.make Unlocked
val mutex : unit -> state Atomic.t = <fun>
```

The `unlock` operation just marks the mutex as unlocked and then wakes up all
the awaiters:

```ocaml
# let rec unlock t =
    match Atomic.exchange t Unlocked with
    | Unlocked -> invalid_arg "mutex: already unlocked"
    | Locked awaiters -> List.iter ((|>) ()) awaiters
val unlock : state Atomic.t -> unit = <fun>
```

The `lock` operation is more complex:

```ocaml
# let rec lock t =
    match Atomic.get t with
    | Unlocked ->
      if not (Atomic.compare_and_set t Unlocked (Locked [])) then
        lock t
    | Locked awaiters as before ->
      let dla = Domain_local_await.prepare_for_await () in
      let after = Locked (dla.release :: awaiters) in
      if Atomic.compare_and_set t before after then
        match dla.await () with
        | () -> lock t
        | exception cancellation_exn ->
          let rec cleanup () =
            match Atomic.get t with
            | Unlocked -> ()
            | Locked awaiters as before ->
              if List.for_all ((==) dla.release) awaiters then
                let after =
                  Locked (List.filter ((!=) dla.release) awaiters)
                in
                if not (Atomic.compare_and_set t before after) then
                  cleanup ()
          in
          cleanup ();
          raise cancellation_exn
      else
        lock t
val lock : state Atomic.t -> unit = <fun>
```

In case the mutex is already locked, domain-local-await is used to `await` until
the mutex is unlocked and the corresponding `release` is called. In case await
raises, `unlock` makes sure to remove the `release` operation from the mutex to
avoid a potential space leak.

Let's then use the mutex in a simple example of increment a counter from
multiple domains:

```ocaml
# let mutex = mutex ()
val mutex : state Atomic.t = <abstr>

# let counter = ref 0
val counter : int ref = {contents = 0}

# let domains = List.init 3 @@ fun _ ->
    Domain.spawn @@ fun () ->
    for _ = 1 to 10000 do
      lock mutex;
      incr counter;
      unlock mutex;
    done
val domains : unit Domain.t list = [<abstr>; <abstr>; <abstr>]

# List.iter Domain.join domains
- : unit = ()

# !counter
- : int = 30000
```

Note that, like with the previous lazy implementation, the above mutex
implementation is intentionally kept relatively simple and can be improved in
various ways. It would make sense to use a
[backoff](https://github.com/ocaml-multicore/backoff) in case of contention. The
representation could also be optimized to reduce memory usage. The above mutex
implementation is also unfair.

## Example: Awaitable atomic locations

Let's implement a simple awaitable atomic location abstraction. An awaitable
location contains both the current value of the location and a list of awaiters,
which are just `unit -> unit` functions:

```ocaml
type 'a awaitable_atomic = ('a * (unit -> unit) list) Atomic.t
```

The constructor of awaitable locations just pairs the initial value with an
empty list of awaiters:

```ocaml
# let awaitable_atomic v : _ awaitable_atomic = Atomic.make (v, [])
val awaitable_atomic : 'a -> 'a awaitable_atomic = <fun>
```

Operations that modify awaitable locations, like `fetch_and_add`, need to call
the awaiters to wake them up after a successful modification:

```ocaml
# let rec fetch_and_add x n =
    let (i, awaiters) as was = Atomic.get x in
      if Atomic.compare_and_set x was (i+n, []) then begin
          List.iter ((|>) ()) awaiters;
          i
        end
      else
        fetch_and_add x n
val fetch_and_add : (int * (unit -> unit) list) Atomic.t -> int -> int =
  <fun>
```

We can also have read-only operations, like `get_as`, that can be used to await
for an awaitable location to have a specific value:

```ocaml
# let rec get_as fn x =
    let (v, awaiters) as was = Atomic.get x in
    match fn v with
    | Some w -> w
    | None ->
      let dla = Domain_local_await.prepare_for_await () in
      if Atomic.compare_and_set x was (v, dla.release :: awaiters) then
        match dla.await () with
        | () -> get_as fn x
        | exception cancelation_exn ->
          let rec cleanup () =
            let (w, awaiters) as was = Atomic.get x in
            if v == w then
              let awaiters = List.filter ((!=) dla.release) awaiters in
              if not (Atomic.compare_and_set x was (w, awaiters))
              then cleanup ()
          in
          cleanup ();
          raise cancelation_exn
      else
        get_as fn x
val get_as : ('a -> 'b option) -> ('a * (unit -> unit) list) Atomic.t -> 'b =
  <fun>
```

Notice that we carefully cleaned up in case the `await` was canceled.

We could, of course, also have operations that potentially awaits for the
location to have an acceptable value before attempting modification. Let's leave
that as an exercise.

To test awaitable locations, let's first create a location:

```ocaml
# let x = awaitable_atomic 0
val x : int awaitable_atomic = <abstr>
```

And let's then create a thread that awaits until the value of the location has
changed and then modifies the value of the location:

```ocaml
# let a_thread =
    ()
    |> Thread.create @@ fun () ->
       get_as (fun x -> if x = 0 then None else Some ()) x;
       fetch_and_add x 21 |> ignore
val a_thread : Thread.t = <abstr>
```

The other thread is now awaiting for the initial modification:

```ocaml
# assert (0 = fetch_and_add x 21)
- : unit = ()
```

And we can await for the thread to perform its modification:

```ocaml
# get_as (fun x -> if x <> 21 then Some x else None) x;
- : int = 42
```

Let's then finish by joining with the other thread:

```ocaml
# Thread.join a_thread
- : unit = ()
```

## Example: Transparently asynchronous IO

As a final example, let's sketch out an implementation of something a bit more
involved &mdash; transparently asynchronous IO. The idea is that we implement
operations such as `read` and `write` on Unix file descriptors in such a way
that they block in a scheduler friendly manner allowing other fibers to run
while waiting for the IO.

But first, we want to perform certain operations atomically. For that purpose we
extend the `Atomic` module with a couple of helpers:

```ocaml version>=5.0.0
module Atomic = struct
  include Stdlib.Atomic

  let rec update t fn =
    let before = Atomic.get t in
    let after = fn before in
    if Atomic.compare_and_set t before after then
      before
    else
      update t fn

  let modify t fn = update t fn |> ignore
end
```

Below is the asynchronous IO module. It exposes `read`, `write`, and `accept`
operations on Unix file descriptors. The operations block in a scheduler
friendly manner. The implementation automatically manages a systhread per domain
that runs a `select` loop, which takes care of awaiting for IO operations to be
immediately executable. The operations on file descriptors communicate with the
`select` loop thread.

```ocaml version>=5.0.0
module Async_io : sig
  open Unix
  val read : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> bytes -> int -> int -> int
  val accept : ?cloexec:bool -> file_descr -> file_descr * sockaddr
end = struct
  module Awaiter = struct
    type t = { file_descr : Unix.file_descr; release : unit -> unit }

    let file_descr_of t = t.file_descr

    let rec signal aws file_descr =
      match aws with
      | [] -> ()
      | aw :: aws ->
          if aw.file_descr == file_descr then
            aw.release ()
          else signal aws file_descr

    let signal_or_wakeup wakeup aws file_descr =
      if file_descr == wakeup then begin
        let n = Unix.read file_descr (Bytes.create 1) 0 1 in
        assert (n = 1)
      end
      else signal aws file_descr

    let reject file_descr =
      List.filter (fun aw -> aw.file_descr != file_descr)
  end

  type state = {
    mutable state : [ `Init | `Locked | `Alive | `Dead ];
    mutable pipe_out : Unix.file_descr;
    reading : Awaiter.t list Atomic.t;
    writing : Awaiter.t list Atomic.t;
  }

  let key =
    Domain.DLS.new_key @@ fun () -> {
      state = `Init;
      pipe_out =
        (* Unfortunately we cannot safely allocate a pipe here,
           so we use stdin as a dummy value. *)
        Unix.stdin;
      reading = Atomic.make [];
      writing = Atomic.make [];
    }

  let[@poll error] try_lock s =
    s.state == `Init && begin
      s.state <- `Locked;
      true
    end

  let needs_init s =
    s.state != `Alive

  let[@poll error] unlock s pipe_out =
    s.pipe_out <- pipe_out;
    s.state <- `Alive

  let wakeup s =
    let n = Unix.write s.pipe_out (Bytes.create 1) 0 1 in
    assert (n = 1)

  let rec init s =
    (* DLS initialization may be run multiple times, so we
       perform more involved initialization here. *)
    if try_lock s then begin
      (* The pipe is used to wake up the select after changing
         the lists of reading and writing file descriptors. *)
      let pipe_inn, pipe_out = Unix.pipe ~cloexec:true () in
      unlock s pipe_out;
      let t =
        ()
        |> Thread.create @@ fun () ->
           (* This is the IO select loop that performs select and
              then wakes up fibers blocked on IO. *)
           while s.state != `Dead do
             let rs, ws, _ =
               Unix.select
                 (pipe_inn
                  :: List.map Awaiter.file_descr_of (Atomic.get s.reading))
                 (List.map Awaiter.file_descr_of (Atomic.get s.writing))
                 []
                 (-1.0)
             in
             List.iter
               (Awaiter.signal_or_wakeup pipe_inn (Atomic.get s.reading))
               rs;
             List.iter (Awaiter.signal (Atomic.get s.writing)) ws;
             Atomic.modify s.reading (List.fold_right Awaiter.reject rs);
             Atomic.modify s.writing (List.fold_right Awaiter.reject ws);
         done;
         Unix.close pipe_inn;
         Unix.close pipe_out
      in
      Domain.at_exit @@ fun () ->
        s.state <- `Dead;
        wakeup s;
        Thread.join t
    end
    else if needs_init s then begin
      Thread.yield ();
      init s;
    end

  let get () =
    let s = Domain.DLS.get key in
    if needs_init s then
      init s;
    s

  let await s r file_descr =
    let Domain_local_await.{ await; release } =
      Domain_local_await.prepare_for_await ()
    in
    let awaiter = Awaiter.{ file_descr; release } in
    Atomic.modify r (List.cons awaiter);
    wakeup s;
    try await ()
    with cancellation_exn ->
      Atomic.modify r (List.filter ((!=) awaiter));
      raise cancellation_exn

  let read file_descr bytes pos len =
    let s = get () in
    await s s.reading file_descr;
    Unix.read file_descr bytes pos len

  let write file_descr bytes pos len =
    let s = get () in
    await s s.writing file_descr;
    Unix.write file_descr bytes pos len

  let accept ?cloexec file_descr =
    let s = get () in
    await s s.reading file_descr;
    Unix.accept ?cloexec file_descr
end
```

To demonstrate that we can perform IO operations without blocking the thread we
implement a very minimalistic effects based toy scheduler. We could also use any
existing scheduler that provides support for domain-local-await
([see](#references)).

```ocaml version>=5.0.0
module Toy_scheduler : sig
  val fiber : (unit -> unit) -> unit
  val run : (unit -> unit) -> unit
end = struct
  type _ Effect.t +=
    | Suspend : (('a, unit) Effect.Deep.continuation -> unit) -> 'a Effect.t

  let ready = Atomic.make []
  let num_alive_fibers = ref 0

  let fiber thunk =
    incr num_alive_fibers;
    let thunk () =
      thunk ();
      decr num_alive_fibers
    in
    Atomic.modify ready (List.cons thunk)

  let run program =
    let needs_wakeup = Atomic.make false in
    let pipe_inn, pipe_out = Unix.pipe ~cloexec:true () in
    let rec scheduler () =
      match Atomic.update ready (function [] -> [] | _::xs -> xs) with
      | work::_ ->
        let effc (type a) : a Effect.t -> _ = function
          | Suspend ef -> Some ef
          | _ -> None in
        Effect.Deep.try_with work () { effc };
        scheduler ()
      | [] ->
        if !num_alive_fibers <> 0 then begin
          if Atomic.get needs_wakeup then
            (* There are blocked fibers, so we wait for them to
               become unblocked. *)
            let _ = Unix.select [pipe_inn] [] [] (-1.0) in
            let n = Unix.read pipe_inn (Bytes.create 1) 0 1 in
            assert (n = 1)
          else
            (* There are blocked fibers, so we need to wait for
               them to become ready.  But we need to check the
               ready list once more before we do so. *)
            Atomic.set needs_wakeup true;
          scheduler ()
        end
    in
    let prepare_for_await _ =
      let state = Atomic.make `Init in
      let release () =
        if Atomic.get state != `Released then
          match Atomic.exchange state `Released with
          | `Awaiting k ->
            let thunk = Effect.Deep.continue k in
            Atomic.modify ready (List.cons thunk);
            if Atomic.get needs_wakeup &&
               Atomic.compare_and_set needs_wakeup true false then
              (* The scheduler is potentially waiting on select,
                 so we need to perform a wakeup. *)
              let n = Unix.write pipe_out (Bytes.create 1) 0 1 in
              assert (n = 1)
          | _ -> () in
      let await () =
        if Atomic.get state != `Released then
          Effect.perform @@ Suspend (fun k ->
            if not (Atomic.compare_and_set state `Init (`Awaiting k)) then
              Effect.Deep.continue k ())
      in
      Domain_local_await.{ release; await } in
    Domain_local_await.using
      ~prepare_for_await
      ~while_running:(fun () ->
        incr num_alive_fibers;
        let program () =
          program ();
          decr num_alive_fibers
        in
        Atomic.modify ready (List.cons program);
        scheduler ())
end
```

The toy scheduler and the async IO implementation do not depend on each other
and, more generally, know nothing about each other. They simply _interoperate_
through the use of domain-local-await!

Finally here is an example program that runs a client and a server fiber that
communicate through sockets:

```ocaml version>=5.0.0
# Toy_scheduler.run @@ fun () ->

  let n = 100 in
  let port =
    Random.self_init ();
    Random.int 1000 + 3000
  in
  let server_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  let () =
    Toy_scheduler.fiber @@ fun () ->
    Printf.printf "  Client running\n%!";
    let socket = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
    Fun.protect ~finally:(fun () -> Unix.close socket) @@ fun () ->
    Unix.connect socket server_addr;
    Printf.printf "  Client connected\n%!";
    let bytes = Bytes.create n in
    let n = Async_io.write socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client wrote %d\n%!" n;
    let n = Async_io.read socket bytes 0 (Bytes.length bytes) in
    Printf.printf "  Client read %d\n%!" n
  in

  let () =
    Toy_scheduler.fiber @@ fun () ->
    Printf.printf "  Server running\n%!";
    let client, _client_addr =
      let socket = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
      Fun.protect ~finally:(fun () -> Unix.close socket) @@ fun () ->
      Unix.set_nonblock socket;
      Unix.bind socket server_addr;
      Unix.listen socket 1;
      Printf.printf "  Server listening\n%!";
      Async_io.accept ~cloexec:true socket
    in
    Fun.protect ~finally:(fun () -> Unix.close client) @@ fun () ->
    Unix.set_nonblock client;
    let bytes = Bytes.create n in
    let n = Async_io.read client bytes 0 (Bytes.length bytes) in
    Printf.printf "  Server read %d\n%!" n;
    let n = Async_io.write client bytes 0 (n / 2) in
    Printf.printf "  Server wrote %d\n%!" n
  in

  Printf.printf "Client server test\n%!"
Client server test
  Server running
  Server listening
  Client running
  Client connected
  Client wrote 100
  Server read 100
  Server wrote 50
  Client read 50
- : unit = ()
```

This proof-of-concept shows that using just domain-local-await and a systhread
we can implement scheduler agnostic transparently asynchronous IO. There is a
lot of room for optimizations and other kinds of improvements.

## References

DLA is used to implement blocking operations by the following libraries:

- [Kcas](https://ocaml-multicore.github.io/kcas/)

DLA support is provided by the following schedulers:

- [Eio](https://github.com/ocaml-multicore/eio) <sup>(>= 0.10)</sup>
- [Domainslib](https://github.com/ocaml-multicore/domainslib) <sup>(>=
  0.5.1)</sup>
- [Moonpool](https://github.com/c-cube/moonpool) <sup>(>= 0.3)</sup>
