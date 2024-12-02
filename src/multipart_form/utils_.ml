(* module StringMap = Map.Make (String) *)

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

let ends_with ~suffix ~suffix_length s =
  let s_length = String.length s in
  s_length >= suffix_length
  && string_eq ~a:s ~a_start:(s_length - suffix_length) ~b:suffix
       ~len:suffix_length

let rec first_matching p = function
  | [] -> None
  | x :: xs ->
    (match p x with
    | Some y -> Some y
    | None -> first_matching p xs)

let[@inline] option_map f = function
  | None -> None
  | Some x -> Some (f x)

let find_common_idx a b =
  let rec go i =
    if i <= 0 then
      None
    else if ends_with ~suffix:b ~suffix_length:i a then
      Some (String.length a - i)
    else
      go (i - 1)
  in
  go (String.length b)

(*
let[@inline] word = function
  | "" -> []
  | w -> [ Some w ]

let split_on_string ~pattern s =
  let pattern_length = String.length pattern in
  let rec go start acc =
    match Stringext.find_from ~start s ~pattern with
    | Some match_start ->
      let before = String.sub s start (match_start - start) in
      let new_acc = (None :: word before) @ acc in
      let new_start = match_start + pattern_length in
      go new_start new_acc
    | None -> word (Stringext.string_after s start) @ acc
  in
  List.rev (go 0 [])

let split_and_process_string ~boundary s =
  let f = function
    | None -> `Delim
    | Some w -> `Word w
  in
  List.map f @@ split_on_string ~pattern:boundary s
  *)
