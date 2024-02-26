let percent_encode ?(skip = fun _ -> false) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c when skip c -> Buffer.add_char buf c
      | ( ' ' | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+'
        | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | '~' ) as c ->
        Printf.bprintf buf "%%%X" (Char.code c)
      | c when Char.code c > 127 -> Printf.bprintf buf "%%%X" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let int_of_hex_nibble = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> invalid_arg "string: invalid hex"

let percent_decode (s : string) : _ option =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  try
    while !i < String.length s do
      match String.get s !i with
      | '%' ->
        if !i + 2 < String.length s then (
          (match
             (int_of_hex_nibble (String.get s (!i + 1)) lsl 4)
             + int_of_hex_nibble (String.get s (!i + 2))
           with
          | n -> Buffer.add_char buf (Char.chr n)
          | exception _ -> raise Exit);
          i := !i + 3
        ) else
          raise Exit (* truncated *)
      | '+' ->
        Buffer.add_char buf ' ';
        incr i (* for query strings *)
      | c ->
        Buffer.add_char buf c;
        incr i
    done;
    Some (Buffer.contents buf)
  with Exit -> None

exception Invalid_query

let find_q_index_ s = String.index s '?'

let get_non_query_path s =
  match find_q_index_ s with
  | i -> String.sub s 0 i
  | exception Not_found -> s

let get_query s : string =
  match find_q_index_ s with
  | i -> String.sub s (i + 1) (String.length s - i - 1)
  | exception Not_found -> ""

let split_query s = get_non_query_path s, get_query s

let split_on_slash s : _ list =
  let l = ref [] in
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    match String.index_from s !i '/' with
    | exception Not_found ->
      if !i < n then (* last component *) l := String.sub s !i (n - !i) :: !l;
      i := n (* done *)
    | j ->
      if j > !i then l := String.sub s !i (j - !i) :: !l;
      i := j + 1
  done;
  List.rev !l

let parse_query s : (_ list, string) result =
  let pairs = ref [] in
  let is_sep_ = function
    | '&' | ';' -> true
    | _ -> false
  in
  let i = ref 0 in
  let j = ref 0 in
  try
    let percent_decode s =
      match percent_decode s with
      | Some x -> x
      | None -> raise Invalid_query
    in
    let parse_pair () =
      let eq = String.index_from s !i '=' in
      let k = percent_decode @@ String.sub s !i (eq - !i) in
      let v = percent_decode @@ String.sub s (eq + 1) (!j - eq - 1) in
      pairs := (k, v) :: !pairs
    in
    while !i < String.length s do
      while !j < String.length s && not (is_sep_ (String.get s !j)) do
        incr j
      done;
      if !j < String.length s then (
        assert (is_sep_ (String.get s !j));
        parse_pair ();
        i := !j + 1;
        j := !i
      ) else (
        parse_pair ();
        i := String.length s (* done *)
      )
    done;
    Ok !pairs
  with
  | Invalid_argument _ | Not_found | Failure _ ->
    Error (Printf.sprintf "error in parse_query for %S: i=%d,j=%d" s !i !j)
  | Invalid_query -> Error ("invalid query string: " ^ s)

let show_sockaddr = function
  | Unix.ADDR_UNIX f -> f
  | Unix.ADDR_INET (inet, port) ->
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr inet) port
