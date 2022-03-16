
type t = {
  mutable bytes: bytes;
  mutable i: int;
}

let create ?(size=4_096) () : t =
  { bytes=Bytes.make size ' '; i=0 }

let size self = self.i
let bytes_slice self = self.bytes
let clear self : unit =
  if Bytes.length self.bytes > 4_096 * 1_024 then (
    self.bytes <- Bytes.make 4096 ' '; (* free big buffer *)
  );
  self.i <- 0

let resize self new_size : unit =
  let new_buf = Bytes.make new_size ' ' in
  Bytes.blit self.bytes 0 new_buf 0 self.i;
  self.bytes <- new_buf

let add_bytes (self:t) s i len : unit =
  if self.i + len >= Bytes.length self.bytes then (
    resize self (self.i + self.i / 2 + len + 10);
  );
  Bytes.blit s i self.bytes self.i len;
  self.i <- self.i + len

let contents (self:t) : string = Bytes.sub_string self.bytes 0 self.i

let contents_and_clear (self:t) : string =
  let x = contents self in
  clear self;
  x
