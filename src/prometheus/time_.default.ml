let[@inline] now_us () =
  let t = Unix.gettimeofday () in
  t *. 1e6 |> ceil
