let percent_encode ?(skip=fun _->false) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c when skip c -> Buffer.add_char buf c
      | ' ' -> Buffer.add_string buf "%20"
      | '!' -> Buffer.add_string buf "%21"
      | '"' -> Buffer.add_string buf "%22"
      | '#' -> Buffer.add_string buf "%23"
      | '$' -> Buffer.add_string buf "%24"
      | '%' -> Buffer.add_string buf "%25"
      | '&' -> Buffer.add_string buf "%26"
      | '\'' -> Buffer.add_string buf "%27"
      | '(' -> Buffer.add_string buf "%28"
      | ')' -> Buffer.add_string buf "%29"
      | '*' -> Buffer.add_string buf "%2A"
      | '+' -> Buffer.add_string buf "%2B"
      | ',' -> Buffer.add_string buf "%2C"
      | '/' -> Buffer.add_string buf "%2F"
      | ':' -> Buffer.add_string buf "%3A"
      | ';' -> Buffer.add_string buf "%3B"
      | '=' -> Buffer.add_string buf "%3D"
      | '?' -> Buffer.add_string buf "%3F"
      | '@' -> Buffer.add_string buf "%40"
      | '[' -> Buffer.add_string buf "%5B"
      | ']' -> Buffer.add_string buf "%5D"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let percent_decode (s:string) : _ option =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  try
    while !i < String.length s do
      match String.get s !i with
      | '%' ->
        if !i+2 < String.length s then (
          begin match String.sub s (!i+1) 2 with
            | "20" -> Buffer.add_char buf ' '
            | "21" -> Buffer.add_char buf '!'
            | "22" -> Buffer.add_char buf '"'
            | "23" -> Buffer.add_char buf '#'
            | "24" -> Buffer.add_char buf '$'
            | "25" -> Buffer.add_char buf '%'
            | "26" -> Buffer.add_char buf '&'
            | "27" -> Buffer.add_char buf '\''
            | "28" -> Buffer.add_char buf '('
            | "29" -> Buffer.add_char buf ')'
            | "2A" -> Buffer.add_char buf '*'
            | "2B" -> Buffer.add_char buf '+'
            | "2C" -> Buffer.add_char buf ','
            | "2F" -> Buffer.add_char buf '/'
            | "3A" -> Buffer.add_char buf ':'
            | "3B" -> Buffer.add_char buf ';'
            | "3D" -> Buffer.add_char buf '='
            | "3F" -> Buffer.add_char buf '?'
            | "40" -> Buffer.add_char buf '@'
            | "5B" -> Buffer.add_char buf '['
            | "5D" -> Buffer.add_char buf ']'
            | _ -> raise Exit
          end;
          i := !i + 3;
        ) else (
          raise Exit (* truncated *)
        )
      | c -> Buffer.add_char buf c; incr i
    done;
    Some (Buffer.contents buf)
  with Exit -> None

