let[@inline] now_us () =
  let t = Mtime_clock.now_ns () in
  Int64.(div t 1000L |> to_float)
