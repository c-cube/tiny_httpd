(*
  https://prometheus.io/docs/instrumenting/exposition_formats/#text-based-format
  *)

open Common_p_

let bpf = Printf.bprintf

type tags = (string * string) list
type counter = { name: string; tags: tags; descr: string option; c: int A.t }
type gauge = { name: string; tags: tags; descr: string option; g: int A.t }

type histogram = {
  name: string;
  tags: tags;
  descr: string option;
  sum: float A.t;
  buckets: (float * int A.t) array;
}

type registry = {
  mutable counters: counter list;
  mutable gauges: gauge list;
  mutable hists: histogram list;
  mutable on_will_emit: (unit -> unit) list;
}

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

let opt_iter_ f = function
  | None -> ()
  | Some x -> f x

module Counter = struct
  type t = counter

  let create (reg : registry) ?(tags = []) ?descr name : t =
    let self : t = { name; descr; tags; c = A.make 0 } in
    opt_iter_ (validate_descr_ "counter") descr;
    reg.counters <- self :: reg.counters;
    self

  let emit buf (self : t) =
    opt_iter_ (bpf buf "# HELP %s %s\n" self.name) self.descr;
    bpf buf "# TYPE %s counter\n" self.name;
    bpf buf "%s%a %d\n" self.name emit_tags_ self.tags (A.get self.c);
    ()

  let[@inline] incr self = A.incr self.c
  let[@inline] incr_by self n = ignore (A.fetch_and_add self.c n : int)

  let incr_to self n =
    while
      let old = A.get self.c in
      if old < n then
        not (A.compare_and_set self.c old n)
      else
        false
    do
      ()
    done
end

module Gauge = struct
  type t = gauge

  let create (reg : registry) ?(tags = []) ?descr name : t =
    opt_iter_ (validate_descr_ "gauge") descr;
    let self : t = { name; descr; tags; g = A.make 0 } in
    reg.gauges <- self :: reg.gauges;
    self

  let emit buf (self : t) =
    opt_iter_ (bpf buf "# HELP %s %s\n" self.name) self.descr;
    bpf buf "# TYPE %s gauge\n" self.name;
    bpf buf "%s%a %d\n" self.name emit_tags_ self.tags (A.get self.g);
    ()

  let[@inline] set self x = A.set self.g x
  let[@inline] incr self = A.incr self.g
  let[@inline] incr_by self n = ignore (A.fetch_and_add self.g n : int)
  let[@inline] decr self = A.decr self.g
  let[@inline] decr_by self n = ignore (A.fetch_and_add self.g (-n) : int)
end

module Histogram = struct
  type t = histogram

  let create reg ?(tags = []) ?descr ~buckets name : t =
    opt_iter_ (validate_descr_ "histogram") descr;
    let buckets =
      List.sort Stdlib.compare buckets
      |> List.map (fun thresh -> thresh, A.make 0)
    in
    let buckets = Array.of_list @@ buckets @ [ infinity, A.make 0 ] in
    let self : t = { name; descr; tags; sum = A.make 0.; buckets } in
    reg.hists <- self :: reg.hists;
    self

  let add (self : t) n =
    while
      let old = A.get self.sum in
      not (A.compare_and_set self.sum old (old +. n))
    do
      ()
    done;
    let i = ref 0 in
    let continue = ref true in
    while !continue && !i < Array.length self.buckets do
      let thresh, count = self.buckets.(!i) in
      if n <= thresh then (
        continue := false;
        A.incr count
      ) else
        incr i
    done

  let emit buf (self : t) : unit =
    opt_iter_ (bpf buf "# HELP %s %s\n" self.name) self.descr;
    bpf buf "# TYPE %s histogram\n" self.name;

    let count = ref 0 in
    for i = 0 to Array.length self.buckets - 1 do
      let thresh, buck_count = self.buckets.(i) in
      count := !count + A.get buck_count;

      let name =
        if thresh = infinity then
          "+Inf"
        else
          string_of_float thresh
      in
      bpf buf "%s_bucket%a %d\n" self.name emit_tags_
        (("le", name) :: self.tags)
        !count
    done;
    bpf buf "%s_count%a %d\n" self.name emit_tags_ self.tags !count;
    bpf buf "%s_sum%a %.3f\n" self.name emit_tags_ self.tags (A.get self.sum);
    ()
end

module Registry = struct
  type t = registry

  let create () : t =
    { counters = []; gauges = []; hists = []; on_will_emit = [] }

  let on_will_emit self f = self.on_will_emit <- f :: self.on_will_emit

  let emit (buf : Buffer.t) (self : t) : unit =
    List.iter (fun f -> f ()) self.on_will_emit;
    List.iter (Gauge.emit buf) self.gauges;
    List.iter (Counter.emit buf) self.counters;
    List.iter (Histogram.emit buf) self.hists;
    ()

  let emit_str (self : t) : string =
    let buf = Buffer.create 32 in
    emit buf self;
    Buffer.contents buf
end

let global = Registry.create ()

let http_middleware (reg : Registry.t) : Server.Middleware.t =
  let c_req =
    Counter.create reg "tiny_httpd_requests" ~descr:"number of HTTP requests"
  in
  let c_err =
    Counter.create reg "tiny_httpd_errors" ~descr:"number of HTTP errors"
  in
  let h_latency =
    Histogram.create reg "tiny_httpd_latency" ~descr:"latency of HTTP responses"
      ~buckets:[ 0.001; 0.01; 0.1; 0.5; 1.; 5.; 10. ]
  in

  fun h : Server.Middleware.handler ->
    fun req ~resp : unit ->
     let start = Time_.now_us () in
     Counter.incr c_req;
     h req ~resp:(fun (response : Response.t) ->
         let code = response.code in

         let elapsed_us = Time_.now_us () -. start in
         let elapsed_s = elapsed_us /. 1e6 in
         Histogram.add h_latency elapsed_s;

         if code < 200 || code >= 400 then Counter.incr c_err;
         resp response)

let add_route_to_server (server : Server.t) (reg : registry) : unit =
  Server.add_route_handler server Route.(exact "metrics" @/ return)
  @@ fun _req ->
  let str = Registry.emit_str reg in
  (* https://prometheus.io/docs/instrumenting/exposition_formats/#text-based-format *)
  let headers = [ "content-type", "text/plain; version=0.0.4" ] in
  Response.make_string ~headers @@ Ok str

let instrument_server (server : Server.t) reg : unit =
  Server.add_middleware ~stage:(`Stage 1) server (http_middleware reg);
  add_route_to_server server reg

module GC_metrics = struct
  type t = { major_coll: counter; major_heap: gauge; compactions: counter }

  let create reg : t =
    let major_coll =
      Counter.create reg ~descr:"major GC collections" "ocaml_gc_major"
    in
    let major_heap =
      Gauge.create reg ~descr:"size of major heap" "ocaml_gc_major_heap_size"
    in
    let compactions =
      Counter.create reg ~descr:"number of GC compactions"
        "ocaml_gc_compactions"
    in
    { major_coll; major_heap; compactions }

  let update (self : t) =
    let stats = Gc.quick_stat () in
    Counter.incr_to self.major_coll stats.major_collections;
    Counter.incr_to self.compactions stats.compactions;
    Gauge.set self.major_heap (stats.heap_words * 8)

  let create_and_update_before_emit reg : unit =
    let gc = create reg in
    Registry.on_will_emit reg (fun () -> update gc)
end
