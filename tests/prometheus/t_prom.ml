module P = Tiny_httpd_prometheus

let pf = Printf.printf
let reg = P.Registry.create ()
let c1 = P.Counter.create reg "t_c1"
let c2 = P.Counter.create reg "t_c2" ~descr:"more awesome than c1"
let g1 = P.Gauge.create reg ~tags:[ "level", "max" ] "yolo_gauge"

let h1 =
  P.Histogram.create reg ~descr:"latency"
    ~buckets:[ 0.01; 0.1; 0.5; 1.; 10. ]
    "latency"

let () =
  print_endline "==== first try ====";
  P.Counter.incr_by c1 42;
  P.Counter.incr c2;
  P.Gauge.set g1 2525;
  P.Histogram.add h1 0.2;
  P.Histogram.add h1 0.003;
  P.Histogram.add h1 0.002;
  P.Histogram.add h1 0.025;
  P.Histogram.add h1 0.9;
  P.Histogram.add h1 7.4;
  P.Histogram.add h1 22.2;
  P.Histogram.add h1 0.3;
  P.Histogram.add h1 0.4;
  P.Histogram.add h1 0.1;

  pf "```\n%s\n```\n" @@ P.Registry.emit_str reg

let () =
  print_endline "==== second try====";
  P.Counter.incr_by c1 11;
  P.Counter.incr c2;
  P.Gauge.set g1 42_000;
  P.Histogram.add h1 23.2;
  P.Histogram.add h1 0.2;

  pf "```\n%s\n```\n" @@ P.Registry.emit_str reg
