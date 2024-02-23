module Buf = Tiny_httpd_buf
module IO = Tiny_httpd_io

let spf = Printf.sprintf

type hidden = unit

type t = {
  mutable bs: bytes;
  mutable off: int;
  mutable len: int;
  fill_buf: unit -> unit;
  consume: int -> unit;
  close: unit -> unit;
  _rest: hidden;
}

let[@inline] close self = self.close ()

let empty =
  {
    bs = Bytes.empty;
    off = 0;
    len = 0;
    fill_buf = ignore;
    consume = ignore;
    close = ignore;
    _rest = ();
  }

let make ?(bs = Bytes.create @@ (16 * 1024)) ?(close = ignore) ~consume ~fill ()
    : t =
  let rec self =
    {
      bs;
      off = 0;
      len = 0;
      close = (fun () -> close self);
      fill_buf = (fun () -> if self.len = 0 then fill self);
      consume =
        (fun n ->
          assert (n <= self.len);
          consume self n);
      _rest = ();
    }
  in
  self

let of_input ?(buf_size = 16 * 1024) (ic : IO.Input.t) : t =
  make ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> IO.Input.close ic)
    ~consume:(fun self n ->
      assert (self.len >= n);
      self.off <- self.off + n;
      self.len <- self.len - n)
    ~fill:(fun self ->
      if self.len = 0 then (
        self.off <- 0;
        self.len <- IO.Input.input ic self.bs 0 (Bytes.length self.bs)
      ))
    ()

let of_chan_ ?buf_size ic ~close_noerr : t =
  let inc = IO.Input.of_in_channel ~close_noerr ic in
  of_input ?buf_size inc

let of_chan ?buf_size ic = of_chan_ ?buf_size ic ~close_noerr:false
let of_chan_close_noerr ?buf_size ic = of_chan_ ?buf_size ic ~close_noerr:true

let of_fd_ ?buf_size ~close_noerr ~closed ic : t =
  let inc = IO.Input.of_unix_fd ~close_noerr ~closed ic in
  of_input ?buf_size inc

let of_fd ?buf_size ~closed fd : t =
  of_fd_ ?buf_size ~closed ~close_noerr:false fd

let of_fd_close_noerr ?buf_size ~closed fd : t =
  of_fd_ ?buf_size ~closed ~close_noerr:true fd

let iter f (self : t) : unit =
  let continue = ref true in
  while !continue do
    self.fill_buf ();
    if self.len = 0 then (
      continue := false;
      self.close ()
    ) else (
      f self.bs self.off self.len;
      self.consume self.len
    )
  done

let to_chan (oc : out_channel) (self : t) = iter (output oc) self
let to_chan' (oc : IO.Output.t) (self : t) = iter (IO.Output.output oc) self

let to_writer (self : t) : Tiny_httpd_io.Writer.t =
  { write = (fun oc -> to_chan' oc self) }

let of_bytes ?(i = 0) ?len (bs : bytes) : t =
  (* invariant: !i+!len is constant *)
  let len =
    match len with
    | Some n ->
      if n > Bytes.length bs - i then invalid_arg "Byte_stream.of_bytes";
      n
    | None -> Bytes.length bs - i
  in
  let self =
    make ~bs ~fill:ignore
      ~close:(fun self -> self.len <- 0)
      ~consume:(fun self n ->
        assert (n >= 0 && n <= self.len);
        self.off <- n + self.off;
        self.len <- self.len - n)
      ()
  in
  self.off <- i;
  self.len <- len;
  self

let of_string s : t = of_bytes (Bytes.unsafe_of_string s)

let with_file ?buf_size file f =
  let ic = Unix.(openfile file [ O_RDONLY ] 0) in
  try
    let x = f (of_fd ?buf_size ~closed:(ref false) ic) in
    Unix.close ic;
    x
  with e ->
    Unix.close ic;
    raise e

let read_all ?(buf = Buf.create ()) (self : t) : string =
  let continue = ref true in
  while !continue do
    self.fill_buf ();
    if self.len = 0 then
      continue := false
    else (
      assert (self.len > 0);
      Buf.add_bytes buf self.bs self.off self.len;
      self.consume self.len
    )
  done;
  Buf.contents_and_clear buf

(* put [n] bytes from the input into bytes *)
let read_exactly_ ~too_short (self : t) (bytes : bytes) (n : int) : unit =
  assert (Bytes.length bytes >= n);
  let offset = ref 0 in
  while !offset < n do
    self.fill_buf ();
    let n_read = min self.len (n - !offset) in
    Bytes.blit self.bs self.off bytes !offset n_read;
    offset := !offset + n_read;
    self.consume n_read;
    if n_read = 0 then too_short ()
  done

(* read a line into the buffer, after clearing it. *)
let read_line_into (self : t) ~buf : unit =
  Buf.clear buf;
  let continue = ref true in
  while !continue do
    self.fill_buf ();
    if self.len = 0 then (
      continue := false;
      if Buf.size buf = 0 then raise End_of_file
    );
    let j = ref self.off in
    while !j < self.off + self.len && Bytes.get self.bs !j <> '\n' do
      incr j
    done;
    if !j - self.off < self.len then (
      assert (Bytes.get self.bs !j = '\n');
      (* line without '\n' *)
      Buf.add_bytes buf self.bs self.off (!j - self.off);
      (* consume line + '\n' *)
      self.consume (!j - self.off + 1);
      continue := false
    ) else (
      Buf.add_bytes buf self.bs self.off self.len;
      self.consume self.len
    )
  done

(* new stream with maximum size [max_size].
   @param close_rec if true, closing this will also close the input stream
   @param too_big called with read size if the max size is reached *)
let limit_size_to ~close_rec ~max_size ~too_big (arg : t) : t =
  let size = ref 0 in
  let continue = ref true in
  make ~bs:Bytes.empty
    ~close:(fun _ -> if close_rec then arg.close ())
    ~fill:(fun res ->
      if res.len = 0 && !continue then (
        arg.fill_buf ();
        res.bs <- arg.bs;
        res.off <- arg.off;
        res.len <- arg.len
      ) else (
        arg.bs <- Bytes.empty;
        arg.off <- 0;
        arg.len <- 0
      ))
    ~consume:(fun res n ->
      size := !size + n;
      if !size > max_size then (
        continue := false;
        too_big !size
      ) else (
        arg.consume n;
        res.off <- res.off + n;
        res.len <- res.len - n
      ))
    ()

(* read exactly [size] bytes from the stream *)
let read_exactly ~close_rec ~size ~too_short (arg : t) : t =
  if size = 0 then
    empty
  else (
    let size = ref size in
    make ~bs:Bytes.empty
      ~fill:(fun res ->
        (* must not block on [arg] if we're done *)
        if !size = 0 then (
          res.bs <- Bytes.empty;
          res.off <- 0;
          res.len <- 0
        ) else (
          arg.fill_buf ();
          res.bs <- arg.bs;
          res.off <- arg.off;
          let len = min arg.len !size in
          if len = 0 && !size > 0 then too_short !size;
          res.len <- len
        ))
      ~close:(fun _res ->
        (* close underlying stream if [close_rec] *)
        if close_rec then arg.close ();
        size := 0)
      ~consume:(fun res n ->
        let n = min n !size in
        size := !size - n;
        arg.consume n;
        res.off <- res.off + n;
        res.len <- res.len - n)
      ()
  )

let read_line ?(buf = Buf.create ()) self : string =
  read_line_into self ~buf;
  Buf.contents buf

let read_chunked ?(buf = Buf.create ()) ~fail (bs : t) : t =
  let first = ref true in
  let read_next_chunk_len () : int =
    if !first then
      first := false
    else (
      let line = read_line ~buf bs in
      if String.trim line <> "" then raise (fail "expected crlf between chunks")
    );
    let line = read_line ~buf bs in
    (* parse chunk length, ignore extensions *)
    let chunk_size =
      if String.trim line = "" then
        0
      else (
        try
          let off = ref 0 in
          let n = Tiny_httpd_parse_.pos_hex line off in
          n
        with _ ->
          raise (fail (spf "cannot read chunk size from line %S" line))
      )
    in
    chunk_size
  in
  let refill = ref true in
  let chunk_size = ref 0 in
  make
    ~bs:(Bytes.create (16 * 4096))
    ~fill:(fun self ->
      (* do we need to refill? *)
      if self.len = 0 then (
        if !chunk_size = 0 && !refill then chunk_size := read_next_chunk_len ();
        self.off <- 0;
        self.len <- 0;
        if !chunk_size > 0 then (
          (* read the whole chunk, or [Bytes.length bytes] of it *)
          let to_read = min !chunk_size (Bytes.length self.bs) in
          read_exactly_
            ~too_short:(fun () -> raise (fail "chunk is too short"))
            bs self.bs to_read;
          self.len <- to_read;
          chunk_size := !chunk_size - to_read
        ) else
          refill := false (* stream is finished *)
      ))
    ~consume:(fun self n ->
      self.off <- self.off + n;
      self.len <- self.len - n)
    ~close:(fun self ->
      (* close this overlay, do not close underlying stream *)
      self.len <- 0;
      refill := false)
    ()

let output_chunked' ?buf (oc : IO.Output.t) (self : t) : unit =
  let oc' = IO.Output.chunk_encoding ?buf oc ~close_rec:false in
  match to_chan' oc' self with
  | () -> IO.Output.close oc'
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    IO.Output.close oc';
    Printexc.raise_with_backtrace e bt

(* print a stream as a series of chunks *)
let output_chunked ?buf (oc : out_channel) (self : t) : unit =
  output_chunked' ?buf (IO.Output.of_out_channel oc) self
