(*
  https://prometheus.io/docs/instrumenting/exposition_formats/#text-based-format
  *)

open Common_

let bpf = Printf.bprintf

type tags = (string * string) list
type counter = { name: string; tags: tags; descr: string option; c: int A.t }
type gauge = { name: string; tags: tags; descr: string option; g: int A.t }
type registry = { mutable counters: counter list; mutable gauges: gauge list }

let validate_descr_ what s =
  if String.contains s '\n' then
    invalid_arg (spf "%s: description cannot contain '\n'" what)

let emit_tags_ buf tags =
  if tags <> [] then (
    bpf buf "{";
    List.iteri
      (fun i (k, v) ->
        if i > 0 then bpf buf ",";
        bpf buf "%s=%S" k v)
      tags;
    bpf buf "}"
  )

module Counter = struct
  type t = counter

  let create (reg : registry) ?(tags = []) ?descr name : t =
    let self : t = { name; descr; tags; c = A.make 0 } in
    Option.iter (validate_descr_ "counter") descr;
    reg.counters <- self :: reg.counters;
    self

  let emit buf (self : t) =
    Option.iter (bpf buf "# HELP %s %s\n" self.name) self.descr;
    bpf buf "# TYPE %s counter\n" self.name;
    bpf buf "%s%a %d\n" self.name emit_tags_ self.tags (A.get self.c);
    ()

  let[@inline] inc self = A.incr self.c
  let[@inline] inc_by self n = ignore (A.fetch_and_add self.c n : int)
  let[@inline] dec self = A.decr self.c
  let[@inline] dec_by self n = ignore (A.fetch_and_add self.c (-n) : int)
end

module Gauge = struct
  type t = gauge

  let create (reg : registry) ?(tags = []) ?descr name : t =
    Option.iter (validate_descr_ "gauge") descr;
    let self : t = { name; descr; tags; g = A.make 0 } in
    reg.gauges <- self :: reg.gauges;
    self

  let emit buf (self : t) =
    Option.iter (bpf buf "# HELP %s %s\n" self.name) self.descr;
    bpf buf "# TYPE %s gauge\n" self.name;
    bpf buf "%s%a %d\n" self.name emit_tags_ self.tags (A.get self.g);
    ()

  let[@inline] set self x = A.set self.g x
  let[@inline] inc self = A.incr self.g
  let[@inline] inc_by self n = ignore (A.fetch_and_add self.g n : int)
  let[@inline] dec self = A.decr self.g
  let[@inline] dec_by self n = ignore (A.fetch_and_add self.g (-n) : int)
end

module Registry = struct
  type t = registry

  let create () : t = { counters = []; gauges = [] }

  let emit (buf : Buffer.t) (self : t) : unit =
    List.iter (Gauge.emit buf) self.gauges;
    List.iter (Counter.emit buf) self.counters;
    ()

  let emit_str (self : t) : string =
    let buf = Buffer.create 32 in
    emit buf self;
    Buffer.contents buf
end

let global = Registry.create ()
