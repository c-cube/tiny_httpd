open Common_

type t = (string * string) list

let empty = []

(* [Char.lowercase_ascii] but easier to inline *)
let[@inline] lower_char_ = function
  | 'A' .. 'Z' as c -> Char.unsafe_chr (Char.code c + 32)
  | c -> c

(** Are these two header names equal? This is case insensitive *)
let equal_name_ (s1 : string) (s2 : string) : bool =
  String.length s1 = String.length s2
  &&
  try
    for i = 0 to String.length s1 - 1 do
      let c1 = String.unsafe_get s1 i |> lower_char_ in
      let c2 = String.unsafe_get s2 i |> lower_char_ in
      if c1 <> c2 then raise_notrace Exit
    done;
    true
  with Exit -> false

let contains name headers =
  List.exists (fun (n, _) -> equal_name_ name n) headers

let list_contains_nocase_ name l = List.exists (equal_name_ name) l

let rec get_exn ?(f = fun x -> x) x h =
  match h with
  | [] -> raise Not_found
  | (k, v) :: _ when equal_name_ x k -> f v
  | _ :: tl -> get_exn ~f x tl

let get ?(f = fun x -> x) x h =
  try Some (get_exn ~f x h) with Not_found -> None

let remove x h = List.filter (fun (k, _) -> not (equal_name_ k x)) h

let set x y h =
  let h =
    if contains x h then
      remove x h
    else
      h
  in
  (x, y) :: h

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

let parse_line_ (line : string) : _ result =
  try
    let i =
      try String.index line ':'
      with Not_found -> failwith "invalid header, missing ':'"
    in
    let k = String.sub line 0 i in
    if not (for_all is_tchar k) then
      failwith (Printf.sprintf "Invalid header key: %S" k);
    let v =
      String.sub line (i + 1) (String.length line - i - 1) |> String.trim
    in
    Ok (k, v)
  with Failure msg -> Error msg

open struct
  let nodup_ = [ "content-length"; "host"; "transfer-encoding" ]
  let is_nodup_ k = list_contains_nocase_ k nodup_
end

let parse_ ~(buf : Buf.t) ?(max_headers = 100) ?(max_header_size = 16 * 1024)
    ?(max_total_size = 256 * 1024) (bs : IO.Input.t) : t =
  let rec loop acc count total_size =
    if count >= max_headers then
      bad_reqf 431 "too many headers (max: %d)" max_headers;
    if total_size >= max_total_size then
      bad_reqf 431 "headers too large (max: %d bytes)" max_total_size;
    match IO.Input.read_line_using_opt ~buf bs with
    | None -> raise End_of_file
    | Some "" -> assert false
    | Some "\r" -> acc
    | Some line when line.[String.length line - 1] <> '\r' ->
      bad_reqf 400 "bad header line, not ended in CRLF"
    | Some line ->
      let line_len = String.length line in
      if line_len > max_header_size then
        bad_reqf 431 "header too large (max: %d bytes)" max_header_size;
      let k, v =
        match parse_line_ line with
        | Ok r -> r
        | Error msg ->
          bad_reqf 400 "invalid header line: %s\nline is: %S" msg line
      in

      if is_nodup_ k && contains k acc then
        bad_reqf 400 "header %S is duplicated" k;

      loop ((k, v) :: acc) (count + 1) (total_size + line_len)
  in

  let headers = loop [] 0 0 in
  if
    contains "content-length" headers
    && get "transfer-encoding" headers = Some "chunked"
  then
    bad_reqf 400 "cannot specify both chunked encoding and content-length";

  headers
