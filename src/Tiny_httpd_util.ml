let percent_encode ?(skip=fun _->false) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c when skip c -> Buffer.add_char buf c
      | (' ' | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+'
        | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | '~')
        as c ->
        Printf.bprintf buf "%%%x" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let hex_int (s:string) : int = Scanf.sscanf s "%x" (fun x->x)

let percent_decode (s:string) : _ option =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  try
    while !i < String.length s do
      match String.get s !i with
      | '%' ->
        if !i+2 < String.length s then (
          begin match hex_int @@ String.sub s (!i+1) 2 with
            | n -> Buffer.add_char buf (Char.chr n)
            | exception _ -> raise Exit
          end;
          i := !i + 3;
        ) else (
          raise Exit (* truncated *)
        )
      | '+' -> Buffer.add_char buf ' '; incr i (* for query strings *)
      | c -> Buffer.add_char buf c; incr i
    done;
    Some (Buffer.contents buf)
  with Exit -> None

