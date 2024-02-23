(** Basic parser for lines *)

type 'a t = string -> int ref -> 'a

open struct
  let spf = Printf.sprintf
end

let[@inline] eof s off = !off = String.length s

let[@inline] skip_space : unit t =
 fun s off ->
  while !off < String.length s && String.unsafe_get s !off = ' ' do
    incr off
  done

let pos_int : int t =
 fun s off : int ->
  skip_space s off;
  let n = ref 0 in
  let continue = ref true in
  while !off < String.length s && !continue do
    match String.unsafe_get s !off with
    | '0' .. '9' as c -> n := (!n * 10) + Char.code c - Char.code '0'
    | ' ' | '\t' | '\n' -> continue := false
    | c -> failwith @@ spf "expected int, got %C" c
  done;
  !n

let pos_hex : int t =
 fun s off : int ->
  skip_space s off;
  let n = ref 0 in
  let continue = ref true in
  while !off < String.length s && !continue do
    match String.unsafe_get s !off with
    | 'a' .. 'f' as c ->
      incr off;
      n := (!n * 16) + Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' as c ->
      incr off;
      n := (!n * 16) + Char.code c - Char.code 'A' + 10
    | '0' .. '9' as c ->
      incr off;
      n := (!n * 16) + Char.code c - Char.code '0'
    | ' ' | '\r' -> continue := false
    | c -> failwith @@ spf "expected int, got %C" c
  done;
  !n

(** Parse a word without spaces *)
let word : string t =
 fun s off ->
  skip_space s off;
  let start = !off in
  let continue = ref true in
  while !off < String.length s && !continue do
    match String.unsafe_get s !off with
    | ' ' | '\r' -> continue := false
    | _ -> incr off
  done;
  if !off = start then failwith "expected word";
  String.sub s start (!off - start)

let exact str : unit t =
 fun s off ->
  skip_space s off;
  let len = String.length str in
  if !off + len > String.length s then
    failwith @@ spf "unexpected EOF, expected %S" str;
  for i = 0 to len - 1 do
    let expected = String.unsafe_get str i in
    let c = String.unsafe_get s (!off + i) in
    if c <> expected then
      failwith @@ spf "expected %S, got %C at position %d" str c i
  done;
  off := !off + len
