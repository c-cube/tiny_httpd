open Common_

type t = (string * string) list

let empty = []

let contains name headers =
  let name' = String.lowercase_ascii name in
  List.exists (fun (n, _) -> name' = n) headers

let get_exn ?(f = fun x -> x) x h =
  let x' = String.lowercase_ascii x in
  List.assoc x' h |> f

let get ?(f = fun x -> x) x h =
  try Some (get_exn ~f x h) with Not_found -> None

let remove x h =
  let x' = String.lowercase_ascii x in
  List.filter (fun (k, _) -> k <> x') h

let set x y h =
  let x' = String.lowercase_ascii x in
  (x', y) :: List.filter (fun (k, _) -> k <> x') h

let pp out l =
  let pp_pair out (k, v) = Format.fprintf out "@[<h>%s: %s@]" k v in
  Format.fprintf out "@[<v>%a@]" (Format.pp_print_list pp_pair) l

(* token = 1*tchar
   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_"
            / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
   Reference: https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 *)
let is_tchar = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
    true
  | _ -> false

let for_all pred s =
  try
    String.iter (fun c -> if not (pred c) then raise Exit) s;
    true
  with Exit -> false

let parse_ ~(buf : Buf.t) ~deadline (bs : #IO.Input_with_timeout.t) : t =
  let rec loop acc =
    match IO.Input_with_timeout.read_line_using_opt ~buf ~deadline bs with
    | None -> raise End_of_file
    | Some "\r" -> acc
    | Some line ->
      let k, v =
        try
          let i = String.index line ':' in
          let k = String.sub line 0 i in
          if not (for_all is_tchar k) then
            invalid_arg (Printf.sprintf "Invalid header key: %S" k);
          let v =
            String.sub line (i + 1) (String.length line - i - 1) |> String.trim
          in
          k, v
        with _ -> bad_reqf 400 "invalid header line: %S" line
      in
      loop ((String.lowercase_ascii k, v) :: acc)
  in
  loop []
