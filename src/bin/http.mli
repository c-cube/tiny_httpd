(* The purpose of this module isn't to be a full blown http parser but rather to
   only parse whatever curl otputs *)
type response =
  { code: int
  ; headers: (string * string) list
  ; body: string
  }

val response : response -> Lexing.lexbuf -> response
