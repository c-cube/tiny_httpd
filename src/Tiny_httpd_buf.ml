
type t = {
  mutable bytes: bytes;
  mutable i: int;
}
type buf = t

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

module Pool = struct
  let b_create_ = create

  type t = {
    buf_size: int;
    mutable n: int;
    mutable bufs: buf list;
  }

  let create ?(buf_size=16 * 1024) () : t =
    { buf_size;
      n=0; bufs=[];
    }

  let alloc self =
    match self.bufs with
    | [] -> b_create_ ~size:self.buf_size ()
    | b :: tl ->
      self.bufs <- tl;
      self.n <- self.n - 1;
      b

  let max_bufs_ = 64 (* do not recycle buffers if we already have that many *)

  let dealloc self b =
    if self.n < max_bufs_ &&
       Bytes.length b.bytes >= self.buf_size
    then (
      clear b;
      self.n <- self.n + 1;
      self.bufs <- b :: self.bufs
    )

end
