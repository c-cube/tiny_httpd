type t = { mutable bytes: bytes; mutable i: int; original: bytes }

let create ?(size = 4_096) () : t =
  let bytes = Bytes.make size ' ' in
  { bytes; i = 0; original = bytes }

let[@inline] size self = self.i
let[@inline] bytes_slice self = self.bytes

let clear self : unit =
  if
    Bytes.length self.bytes > 500 * 1_024
    && Bytes.length self.bytes > Bytes.length self.original
  then
    (* free big buffer *)
    self.bytes <- self.original;
  self.i <- 0

let clear_and_zero self =
  clear self;
  Bytes.fill self.bytes 0 (Bytes.length self.bytes) '\x00'

let resize self new_size : unit =
  let new_buf = Bytes.make new_size ' ' in
  Bytes.blit self.bytes 0 new_buf 0 self.i;
  self.bytes <- new_buf

let add_char self c : unit =
  if self.i + 1 >= Bytes.length self.bytes then
    resize self (self.i + (self.i / 2) + 10);
  Bytes.set self.bytes self.i c;
  self.i <- 1 + self.i

let add_bytes (self : t) s i len : unit =
  if self.i + len >= Bytes.length self.bytes then
    resize self (self.i + (self.i / 2) + len + 10);
  Bytes.blit s i self.bytes self.i len;
  self.i <- self.i + len

let[@inline] add_string self str : unit =
  add_bytes self (Bytes.unsafe_of_string str) 0 (String.length str)

let add_buffer (self : t) (buf : Buffer.t) : unit =
  let len = Buffer.length buf in
  if self.i + len >= Bytes.length self.bytes then
    resize self (self.i + (self.i / 2) + len + 10);
  Buffer.blit buf 0 self.bytes self.i len;
  self.i <- self.i + len

let contents (self : t) : string = Bytes.sub_string self.bytes 0 self.i

let contents_and_clear (self : t) : string =
  let x = contents self in
  clear self;
  x
