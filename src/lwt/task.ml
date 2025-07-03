module ED = Effect.Deep

type _ Effect.t += Await : 'a Lwt.t -> 'a Effect.t

(** Queue of microtasks that are ready *)
let tasks : (unit -> unit) Queue.t = Queue.create ()

let[@inline] push_task f : unit = Queue.push f tasks

let on_uncaught_exn : (exn -> Printexc.raw_backtrace -> unit) ref =
  ref (fun exn bt ->
      Printf.eprintf "lwt_task: uncaught task exception:\n%s\n%s\n%!"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string bt))

let run_all_tasks () : unit =
  let n_processed = ref 0 in
  let max_number_of_steps = min 10_000 (2 * Queue.length tasks) in
  while (not (Queue.is_empty tasks)) && !n_processed < max_number_of_steps do
    let t = Queue.pop tasks in
    incr n_processed;
    try t ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      !on_uncaught_exn exn bt
  done;
  (* make sure we don't sleep forever if there's no lwt promise
    ready but [tasks] contains ready tasks *)
  if not (Queue.is_empty tasks) then ignore (Lwt.pause () : unit Lwt.t)

let () =
  let _hook1 = Lwt_main.Enter_iter_hooks.add_first run_all_tasks in
  let _hook2 = Lwt_main.Leave_iter_hooks.add_first run_all_tasks in
  ()

let await (fut : 'a Lwt.t) : 'a =
  match Lwt.state fut with
  | Lwt.Return x -> x
  | Lwt.Fail exn -> raise exn
  | Lwt.Sleep -> Effect.perform (Await fut)

(** the main effect handler *)
let handler : _ ED.effect_handler =
  let effc : type b. b Effect.t -> ((b, unit) ED.continuation -> 'a) option =
    function
    | Await fut ->
      Some
        (fun k ->
          Lwt.on_any fut
            (fun res -> push_task (fun () -> ED.continue k res))
            (fun exn -> push_task (fun () -> ED.discontinue k exn)))
    | _ -> None
  in

  { effc }

let run_inside_effect_handler_ (type a) (promise : a Lwt.u) f () : unit =
  let res = ref (Error (Failure "not resolved")) in
  let run_f_and_set_res () =
    (try
       let r = f () in
       res := Ok r
     with exn -> res := Error exn);
    Lwt.wakeup_result promise !res
  in
  ED.try_with run_f_and_set_res () handler

let run f : _ Lwt.t =
  let lwt, resolve = Lwt.wait () in
  push_task (run_inside_effect_handler_ resolve f);
  lwt

let run_async f : unit = ignore (run f : unit Lwt.t)

(* TODO: yield, use that in loops? *)
