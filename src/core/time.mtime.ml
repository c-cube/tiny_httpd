let[@inline] now_s () =
  let t = Mtime_clock.now_ns () in
  Int64.(div t 1_000_000_000L |> to_float)

let[@inline] now_us () =
  let t = Mtime_clock.now_ns () in
  Int64.(div t 1000L |> to_float)
