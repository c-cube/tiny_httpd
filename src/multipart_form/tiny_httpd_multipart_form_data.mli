type st

val create : ?buf_size:int -> delim:string -> #Iostream.In.t -> st

(**/*)
module Private_ : sig
  type chunk = Delim | Eof | Read of int

  val read_chunk_ : st -> bytes -> int -> int -> chunk
end
(**/*)
