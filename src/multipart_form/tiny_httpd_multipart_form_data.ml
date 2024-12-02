(* ported from https://github.com/cryptosense/multipart-form-data . *)

open Tiny_httpd

type st = {
  delim: string;
  ic: Iostream.In.t;
  buf_split: bytes;  (** Used to split on the delimiter *)
  mutable buf_len: int;
  buf_line: Buf.t;
  mutable eof: bool;
}

let create ?(buf_size = 64 * 1024) ~delim ic : st =
  let ic = (ic : #Iostream.In.t :> Iostream.In.t) in
  {
    delim;
    ic;
    buf_split = Bytes.create buf_size;
    buf_len = 0;
    buf_line = Buf.create ~size:256 ();
    eof = false;
  }

type chunk = Delim | Eof | Read of int

let[@inline] min_len_ (self : st) : int = 2 + String.length self.delim

let shift_left_ (self : st) n =
  if n = self.buf_len then
    self.buf_len <- 0
  else (
    assert (n < self.buf_len);
    Bytes.blit self.buf_split n self.buf_split 0 (self.buf_len - n);
    self.buf_len <- self.buf_len - n
  )

exception Found_delim of int

let rec read_chunk_ (self : st) buf i_buf len : chunk =
  if self.eof then
    Eof
  else if self.buf_len < min_len_ self then (
    (* try to refill buffer *)
    let n =
      Iostream.In.input self.ic self.buf_split self.buf_len
        (Bytes.length self.buf_split - self.buf_len)
    in
    Printf.eprintf "refill n=%d\n%!" n;
    if n = 0 && self.buf_len = 0 then (
      self.eof <- true;
      Eof
    ) else if n = 0 then (
      let n_read = min len self.buf_len in
      Bytes.blit self.buf_split 0 buf i_buf n_read;
      shift_left_ self n_read;
      Read n_read
    ) else (
      self.buf_len <- self.buf_len + n;
      read_chunk_ self buf i_buf len
    )
  ) else (
    Printf.eprintf "normal path buflen=%d buf=%S\n%!" self.buf_len
      (Bytes.sub_string self.buf_split 0 self.buf_len);
    try
      let i = ref 0 in
      let end_pos = min len self.buf_len - 2 - String.length self.delim in
      while !i <= end_pos do
        Printf.eprintf "at %d\n%!" !i;
        if
          Bytes.unsafe_get self.buf_split !i = '-'
          && Bytes.unsafe_get self.buf_split (!i + 1) = '-'
          && Utils_.string_eq
               ~a:(Bytes.unsafe_to_string self.buf_split)
               ~a_start:(!i + 2) ~b:self.delim ~len:(String.length self.delim)
        then
          raise_notrace (Found_delim !i);
        incr i
      done;
      let n_read = min !i len in
      Bytes.blit self.buf_split 0 buf i_buf n_read;
      shift_left_ self n_read;
      Read n_read
    with
    | Found_delim 0 ->
      Printf.eprintf "found delim at 0\n%!";
      shift_left_ self (2 + String.length self.delim);
      Delim
    | Found_delim n ->
      Printf.eprintf "found delim at %d\n%!" n;
      let n_read = min n len in
      Bytes.blit self.buf_split 0 buf i_buf n_read;
      shift_left_ self n_read;
      Read n_read
  )

module Private_ = struct
  type nonrec chunk = chunk = Delim | Eof | Read of int

  let read_chunk_ = read_chunk_
end
