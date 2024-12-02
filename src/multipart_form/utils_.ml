let string_eq ~a ~a_start ~b ~len : bool =
  assert (len <= String.length b);
  if String.length a >= a_start + len then (
    try
      for i = 0 to len - 1 do
        let a_i = a_start + i in
        if String.unsafe_get a a_i <> String.unsafe_get b i then
          raise_notrace Exit
      done;
      true
    with Exit -> false
  ) else
    false

let split1_on ~c s =
  match String.index s c with
  | exception Not_found -> None
  | i -> Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
