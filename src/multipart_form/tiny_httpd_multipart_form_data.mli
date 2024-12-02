(** Parser for multipart/form-data *)
type st
(** Parser state *)

val create :
  ?buf_size:int -> ?out_buf_size:int -> boundary:string -> #Iostream.In.t -> st

type slice = Iostream.Slice.t
type event = Part of Tiny_httpd.Headers.t | Read of slice | End_of_input

val next : st -> event

(**/*)
module Private_ : sig
  type chunk = Delim | Eof | Read of int

  val read_chunk_ : st -> bytes -> int -> int -> chunk
end
(**/*)
