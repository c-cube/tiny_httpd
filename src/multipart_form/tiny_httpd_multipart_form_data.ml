(* ported from https://github.com/cryptosense/multipart-form-data . *)

open Tiny_httpd
module Slice = Iostream.Slice

let spf = Printf.sprintf

type buf = { bs: bytes; mutable len: int }

let shift_left_ (self : buf) n =
  if n = self.len then
    self.len <- 0
  else (
    assert (n < self.len);
    Bytes.blit self.bs n self.bs 0 (self.len - n);
    self.len <- self.len - n
  )

let[@inline] buf_full (self : buf) : bool = self.len >= Bytes.length self.bs

type slice = Iostream.Slice.t
type event = Part of Tiny_httpd.Headers.t | Read of slice | End_of_input
type out_state = Begin | Inside_part | Eof

type st = {
  boundary: string;
  ic: Iostream.In.t;
  buf: buf;  (** Used to split on the boundary *)
  mutable eof_split: bool;
  buf_out: buf;  (** Used to return output slices *)
  mutable st_out: out_state;
}

let create ?(buf_size = 64 * 1024) ?(out_buf_size = 8 * 1024) ~boundary ic : st
    =
  let ic = (ic : #Iostream.In.t :> Iostream.In.t) in
  {
    boundary;
    ic;
    buf = { bs = Bytes.create buf_size; len = 0 };
    eof_split = false;
    buf_out = { bs = Bytes.create out_buf_size; len = 0 };
    st_out = Begin;
  }

type chunk = Delim | Eof | Read of int

let[@inline] min_len_ (self : st) : int = 4 + String.length self.boundary

exception Found_boundary of int

let rec read_chunk_ (self : st) buf i_buf len : chunk =
  if self.eof_split then
    Eof
  else if self.buf.len < min_len_ self then (
    (* try to refill buffer *)
    let n =
      Iostream.In.input self.ic self.buf.bs self.buf.len
        (Bytes.length self.buf.bs - self.buf.len)
    in
    if n = 0 && self.buf.len = 0 then (
      self.eof_split <- true;
      Eof
    ) else if n = 0 then (
      let n_read = min len self.buf.len in
      Bytes.blit self.buf.bs 0 buf i_buf n_read;
      shift_left_ self.buf n_read;
      Read n_read
    ) else (
      self.buf.len <- self.buf.len + n;
      read_chunk_ self buf i_buf len
    )
  ) else (
    try
      let i = ref 0 in
      let end_pos = min len self.buf.len - 4 - String.length self.boundary in
      while !i <= end_pos do
        if
          Bytes.unsafe_get self.buf.bs !i = '\r'
          && Bytes.unsafe_get self.buf.bs (!i + 1) = '\n'
          && Bytes.unsafe_get self.buf.bs (!i + 2) = '-'
          && Bytes.unsafe_get self.buf.bs (!i + 3) = '-'
          && Utils_.string_eq
               ~a:(Bytes.unsafe_to_string self.buf.bs)
               ~a_start:(!i + 4) ~b:self.boundary
               ~len:(String.length self.boundary)
        then
          raise_notrace (Found_boundary !i);
        incr i
      done;
      let n_read = min !i len in
      Bytes.blit self.buf.bs 0 buf i_buf n_read;
      shift_left_ self.buf n_read;
      Read n_read
    with
    | Found_boundary 0 ->
      shift_left_ self.buf (4 + String.length self.boundary);
      Delim
    | Found_boundary n ->
      let n_read = min n len in
      Bytes.blit self.buf.bs 0 buf i_buf n_read;
      shift_left_ self.buf n_read;
      Read n_read
  )

exception Found of int

(** Find \r\n *)
let find_crlf_exn (buf : buf) : int =
  try
    for i = 0 to buf.len - 2 do
      if
        Bytes.unsafe_get buf.bs i = '\r'
        && Bytes.unsafe_get buf.bs (i + 1) = '\n'
      then
        raise_notrace (Found i)
    done;
    raise Not_found
  with Found i -> i

let[@inline] read_to_buf_out_ (self : st) =
  assert (not (buf_full self.buf_out));
  read_chunk_ self self.buf_out.bs self.buf_out.len
    (Bytes.length self.buf_out.bs - self.buf_out.len)

let read_data_or_fail_ (self : st) : unit =
  match read_to_buf_out_ self with
  | Delim -> failwith "multipart: unexpected boundary while parsing headers"
  | Eof -> failwith "multipart: unexpected EOF while parsing headers"
  | Read n -> self.buf_out.len <- self.buf_out.len + n

let rec next (self : st) : event =
  match self.st_out with
  | Eof -> End_of_input
  | Inside_part when self.buf_out.len > 0 ->
    (* there's data to return *)
    let sl =
      { Slice.bytes = self.buf_out.bs; off = 0; len = self.buf_out.len }
    in
    self.buf_out.len <- 0;
    Read sl
  | Inside_part ->
    (* refill or reach boundary *)
    (match read_to_buf_out_ self with
    | Eof ->
      self.st_out <- Eof;
      End_of_input
    | Delim -> parse_after_boundary self
    | Read n ->
      self.buf_out.len <- n;
      next self)
  | Begin ->
    (match read_to_buf_out_ self with
    | Delim -> parse_after_boundary self
    | Eof ->
      self.st_out <- Eof;
      End_of_input
    | Read _ -> failwith "multipart: expected boundary, got data")

and parse_after_boundary (self : st) : event =
  while self.buf_out.len < 2 do
    read_data_or_fail_ self
  done;

  let after_boundary = Bytes.sub_string self.buf_out.bs 0 2 in
  shift_left_ self.buf_out 2;
  match after_boundary with
  | "--" ->
    self.st_out <- Eof;
    End_of_input
  | "\r\n" ->
    let headers = parse_headers_rec self [] in
    self.st_out <- Inside_part;
    Part headers
  | s ->
    failwith (spf "multipart: expect '--' or '\r\n' after boundary, got %S" s)

and parse_headers_rec (self : st) acc : Headers.t =
  if self.buf_out.len = 0 then (
    read_data_or_fail_ self;
    parse_headers_rec self acc
  ) else (
    match find_crlf_exn self.buf_out with
    | exception Not_found ->
      if buf_full self.buf_out then
        failwith "multipart: header line is too long"
      else (
        read_data_or_fail_ self;
        parse_headers_rec self acc
      )
    | i ->
      let line = Bytes.sub_string self.buf_out.bs 0 i in
      Printf.eprintf "parse header line %S\n%!" line;
      shift_left_ self.buf_out (i + 2);
      if line = "" then
        List.rev acc
      else (
        match Tiny_httpd.Headers.parse_line_ line with
        | Ok (k, v) ->
          parse_headers_rec self ((String.lowercase_ascii k, v) :: acc)
        | Error msg ->
          failwith
            (spf "multipart: failed to parser header: %s\nline: %S" msg line)
      )
  )

let parse_content_type (hs : Tiny_httpd.Headers.t) : _ option =
  match Tiny_httpd.Headers.get "content-type" hs with
  | None -> None
  | Some s ->
    (match String.split_on_char ';' s with
    | "multipart/form-data" :: tl ->
      let boundary = ref None in
      List.iter
        (fun s ->
          match Utils_.split1_on ~c:'=' @@ String.trim s with
          | Some ("boundary", "") -> ()
          | Some ("boundary", s) ->
            let s =
              if s.[0] = '"' && s.[String.length s - 1] = '"' then
                String.sub s 1 (String.length s - 2)
              else
                s
            in
            boundary := Some (`boundary s)
          | _ -> ())
        tl;
      !boundary
    | _ -> None)

module Private_ = struct
  type nonrec chunk = chunk = Delim | Eof | Read of int

  let read_chunk_ = read_chunk_
end
