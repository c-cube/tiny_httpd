module P = Tiny_httpd_prometheus

let pf = Printf.printf
let reg = P.Registry.create ()
let c1 = P.Counter.create reg "t_c1"
let c2 = P.Counter.create reg "t_c2" ~descr:"more awesome than c1"
let g1 = P.Gauge.create reg ~tags:[ "level", "max" ] "yolo_gauge"

let () =
  print_endline "==== first try ====";
  P.Counter.incr_by c1 42;
  P.Counter.incr c2;
  P.Gauge.set g1 2525;

  pf "```\n%s\n```\n" @@ P.Registry.emit_str reg

let () =
  print_endline "==== second try====";
  P.Counter.incr_by c1 11;
  P.Counter.incr c2;
  P.Gauge.set g1 42_000;

  pf "```\n%s\n```\n" @@ P.Registry.emit_str reg
