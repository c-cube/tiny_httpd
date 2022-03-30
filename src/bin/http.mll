{

type response =
  { code: int
  ; headers: (string * string) list
  ; body: string
  }

let add_code t code =
  { t with code = int_of_string code }

let add_header t key val_ =
  { t with headers = (key, (String.trim val_)) :: t.headers }

let add_body t b = { t with body=Buffer.contents b }

let set_lexeme_length buf n =
  let open Lexing in
  if n < 0 then
    invalid_arg "set_lexeme_length: offset should be positive";
  if n > buf.lex_curr_pos - buf.lex_start_pos then
    invalid_arg "set_lexeme_length: offset larger than lexeme";
  buf.lex_curr_pos <- buf.lex_start_pos + n;
  buf.lex_curr_p <- {
    buf.lex_start_p
    with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos
  }
}

let space = [' ' '\t']

rule response resp = parse
  | [^ ' ']+ { code resp lexbuf }
and code resp = parse
  | space+ (['0' - '9']+ as code) { status (add_code resp code) lexbuf }
and status resp = parse
  | space+  { status resp lexbuf }
  | [^ '\n']+ '\n' { header_start resp lexbuf }
and header_start resp = parse
  | "\r\n" { body resp (Buffer.create 128) lexbuf }
  | space+ { header_start resp lexbuf }
  | _ {
      set_lexeme_length lexbuf 0;
      header_key resp lexbuf
    }
and header_key resp = parse
  | space* ([^ ':']+ as key) space* ':' space* { header_val resp key lexbuf }
and header_val resp key = parse
  | ([^ '\n']+ as v) '\n' { header_start (add_header resp key v) lexbuf }
and body resp b = parse
  | eof { add_body resp b }
  | _ as c { Buffer.add_char b c; body resp b lexbuf }
