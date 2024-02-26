(** IO abstraction.

    We abstract IO so we can support classic unix blocking IOs
    with threads, and modern async IO with Eio.

    {b NOTE}: experimental.

    @since 0.14
*)

open Common_
module Buf = Buf
module Slice = Iostream.Slice

(** Output channel (byte sink) *)
module Output = struct
  include Iostream.Out_buf

  class of_unix_fd ?(close_noerr = false) ~closed ~(buf : Slice.t)
    (fd : Unix.file_descr) : t =
    object
      inherit t_from_output ~bytes:buf.bytes ()

      method private output_underlying bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          match Unix.write fd bs !i !len with
          | 0 -> failwith "write failed"
          | n ->
            i := !i + n;
            len := !len - n
          | exception
              Unix.Unix_error
                ( ( Unix.EBADF | Unix.ENOTCONN | Unix.ESHUTDOWN
                  | Unix.ECONNRESET | Unix.EPIPE ),
                  _,
                  _ ) ->
            failwith "write failed"
          | exception
              Unix.Unix_error
                ((Unix.EWOULDBLOCK | Unix.EAGAIN | Unix.EINTR), _, _) ->
            ignore (Unix.select [] [ fd ] [] 1.)
        done

      method private close_underlying () =
        if not !closed then (
          closed := true;
          if close_noerr then (
            try Unix.close fd with _ -> ()
          ) else
            Unix.close fd
        )
    end

  let output_buf (self : t) (buf : Buf.t) : unit =
    let b = Buf.bytes_slice buf in
    output self b 0 (Buf.size buf)

  (** [chunk_encoding oc] makes a new channel that outputs its content into [oc]
      in chunk encoding form.
      @param close_rec if true, closing the result will also close [oc]
      @param buf a buffer used to accumulate data into chunks.
        Chunks are emitted when [buf]'s size gets over a certain threshold,
        or when [flush] is called.
      *)
  let chunk_encoding ?(buf = Buf.create ()) ~close_rec (oc : #t) : t =
    (* write content of [buf] as a chunk if it's big enough.
       If [force=true] then write content of [buf] if it's simply non empty. *)
    let write_buf ~force () =
      let n = Buf.size buf in
      if (force && n > 0) || n >= 4_096 then (
        output_string oc (Printf.sprintf "%x\r\n" n);
        output oc (Buf.bytes_slice buf) 0 n;
        output_string oc "\r\n";
        Buf.clear buf
      )
    in

    object
      method flush () =
        write_buf ~force:true ();
        flush oc

      method close () =
        write_buf ~force:true ();
        (* write an empty chunk to close the stream *)
        output_string oc "0\r\n";
        (* write another crlf after the stream (see #56) *)
        output_string oc "\r\n";
        flush oc;
        if close_rec then close oc

      method output b i n =
        Buf.add_bytes buf b i n;
        write_buf ~force:false ()

      method output_char c =
        Buf.add_char buf c;
        write_buf ~force:false ()
    end
end

(** Input channel (byte source) *)
module Input = struct
  include Iostream.In_buf

  let of_unix_fd ?(close_noerr = false) ~closed ~(buf : Slice.t)
      (fd : Unix.file_descr) : t =
    let eof = ref false in
    object
      inherit Iostream.In_buf.t_from_refill ~bytes:buf.bytes ()

      method private refill (slice : Slice.t) =
        if not !eof then (
          slice.off <- 0;
          let continue = ref true in
          while !continue do
            match Unix.read fd slice.bytes 0 (Bytes.length slice.bytes) with
            | n ->
              slice.len <- n;
              continue := false
            | exception
                Unix.Unix_error
                  ( ( Unix.EBADF | Unix.ENOTCONN | Unix.ESHUTDOWN
                    | Unix.ECONNRESET | Unix.EPIPE ),
                    _,
                    _ ) ->
              eof := true;
              continue := false
            | exception
                Unix.Unix_error
                  ((Unix.EWOULDBLOCK | Unix.EAGAIN | Unix.EINTR), _, _) ->
              ignore (Unix.select [ fd ] [] [] 1.)
          done;
          (* Printf.eprintf "read returned %d B\n%!" !n; *)
          if slice.len = 0 then eof := true
        )

      method close () =
        if not !closed then (
          closed := true;
          eof := true;
          if close_noerr then (
            try Unix.close fd with _ -> ()
          ) else
            Unix.close fd
        )
    end

  let of_slice (slice : Slice.t) : t =
    object
      inherit Iostream.In_buf.t_from_refill ~bytes:slice.bytes ()

      method private refill (slice : Slice.t) =
        slice.off <- 0;
        slice.len <- 0

      method close () = ()
    end

  (** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
  let[@inline] input (self : t) buf i len = self#input buf i len

  (** Close the channel. *)
  let[@inline] close self : unit = self#close ()

  (** Read exactly [len] bytes.
      @raise End_of_file if the input did not contain enough data. *)
  let really_input (self : t) buf i len : unit =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      let n = input self buf !i !len in
      if n = 0 then raise End_of_file;
      i := !i + n;
      len := !len - n
    done

  let append (i1 : #t) (i2 : #t) : t =
    let use_i1 = ref true in
    let rec input_rec (slice : Slice.t) =
      if !use_i1 then (
        slice.len <- input i1 slice.bytes 0 (Bytes.length slice.bytes);
        if slice.len = 0 then (
          use_i1 := false;
          input_rec slice
        )
      ) else
        slice.len <- input i1 slice.bytes 0 (Bytes.length slice.bytes)
    in

    object
      inherit Iostream.In_buf.t_from_refill ()

      method private refill (slice : Slice.t) =
        slice.off <- 0;
        input_rec slice

      method close () =
        close i1;
        close i2
    end

  let iter_slice (f : Slice.t -> unit) (self : #t) : unit =
    let continue = ref true in
    while !continue do
      let slice = self#fill_buf () in
      if slice.len = 0 then (
        continue := false;
        close self
      ) else (
        f slice;
        Slice.consume slice slice.len
      )
    done

  let iter f self =
    iter_slice (fun (slice : Slice.t) -> f slice.bytes slice.off slice.len) self

  let to_chan oc (self : #t) =
    iter_slice
      (fun (slice : Slice.t) ->
        Stdlib.output oc slice.bytes slice.off slice.len)
      self

  let to_chan' (oc : #Iostream.Out.t) (self : #t) : unit =
    iter_slice
      (fun (slice : Slice.t) ->
        Iostream.Out.output oc slice.bytes slice.off slice.len)
      self

  let read_all_using ~buf (self : #t) : string =
    Buf.clear buf;
    let continue = ref true in
    while !continue do
      let slice = self#fill_buf () in
      if slice.len = 0 then
        continue := false
      else (
        assert (slice.len > 0);
        Buf.add_bytes buf slice.bytes slice.off slice.len;
        Slice.consume slice slice.len
      )
    done;
    Buf.contents_and_clear buf

  (** put [n] bytes from the input into bytes *)
  let read_exactly_ ~too_short (self : #t) (bytes : bytes) (n : int) : unit =
    assert (Bytes.length bytes >= n);
    let offset = ref 0 in
    while !offset < n do
      let slice = self#fill_buf () in
      let n_read = min slice.len (n - !offset) in
      Bytes.blit slice.bytes slice.off bytes !offset n_read;
      offset := !offset + n_read;
      Slice.consume slice n_read;
      if n_read = 0 then too_short ()
    done

  (** read a line into the buffer, after clearing it. *)
  let read_line_into (self : t) ~buf : unit =
    Buf.clear buf;
    let continue = ref true in
    while !continue do
      let slice = self#fill_buf () in
      if slice.len = 0 then (
        continue := false;
        if Buf.size buf = 0 then raise End_of_file
      );
      let j = ref slice.off in
      while !j < slice.off + slice.len && Bytes.get slice.bytes !j <> '\n' do
        incr j
      done;
      if !j - slice.off < slice.len then (
        assert (Bytes.get slice.bytes !j = '\n');
        (* line without '\n' *)
        Buf.add_bytes buf slice.bytes slice.off (!j - slice.off);
        (* consume line + '\n' *)
        Slice.consume slice (!j - slice.off + 1);
        continue := false
      ) else (
        Buf.add_bytes buf slice.bytes slice.off slice.len;
        Slice.consume slice slice.len
      )
    done

  let read_line_using ~buf (self : #t) : string =
    read_line_into self ~buf;
    Buf.contents_and_clear buf

  let read_line_using_opt ~buf (self : #t) : string option =
    match read_line_into self ~buf with
    | () -> Some (Buf.contents_and_clear buf)
    | exception End_of_file -> None

  let reading_exactly_ ~skip_on_close ~close_rec ~size (arg : t) : t =
    let remaining_size = ref size in

    object
      method close () =
        if !remaining_size > 0 && skip_on_close then skip arg !remaining_size;
        if close_rec then close arg

      method fill_buf () =
        if !remaining_size > 0 then
          fill_buf arg
        else
          Slice.empty

      method input bs i len =
        if !remaining_size > 0 then (
          let slice = fill_buf arg in
          let n = min len (min slice.len !remaining_size) in
          Bytes.blit slice.bytes slice.off bs i n;
          remaining_size := !remaining_size - n;
          Slice.consume slice n;
          n
        ) else
          0

      method consume n =
        if n > !remaining_size then
          invalid_arg "reading_exactly: consuming too much";
        remaining_size := !remaining_size - n;
        consume arg n
    end

  (** new stream with maximum size [max_size].
   @param close_rec if true, closing this will also close the input stream *)
  let limit_size_to ~close_rec ~max_size (arg : t) : t =
    reading_exactly_ ~size:max_size ~skip_on_close:false ~close_rec arg

  (** New stream that consumes exactly [size] bytes from the input.
        If fewer bytes are read before [close] is called, we read and discard
        the remaining quota of bytes before [close] returns.
   @param close_rec if true, closing this will also close the input stream *)
  let reading_exactly ~close_rec ~size (arg : t) : t =
    reading_exactly_ ~size ~close_rec ~skip_on_close:true arg

  let read_chunked ~(bytes : bytes) ~fail (ic : #t) : t =
    let first = ref true in

    (* small buffer to read the chunk sizes *)
    let line_buf = Buf.create ~size:32 () in
    let read_next_chunk_len () : int =
      if !first then
        first := false
      else (
        let line = read_line_using ~buf:line_buf ic in
        if String.trim line <> "" then
          raise (fail "expected crlf between chunks")
      );
      let line = read_line_using ~buf:line_buf ic in
      (* parse chunk length, ignore extensions *)
      let chunk_size =
        if String.trim line = "" then
          0
        else (
          try
            let off = ref 0 in
            let n = Parse_.pos_hex line off in
            n
          with _ ->
            raise (fail (spf "cannot read chunk size from line %S" line))
        )
      in
      chunk_size
    in
    let eof = ref false in
    let chunk_size = ref 0 in

    object
      inherit t_from_refill ~bytes ()

      method private refill (slice : Slice.t) : unit =
        if !chunk_size = 0 && not !eof then (
          chunk_size := read_next_chunk_len ();
          if !chunk_size = 0 then eof := true (* stream is finished *)
        );
        slice.off <- 0;
        slice.len <- 0;
        if !chunk_size > 0 then (
          (* read the whole chunk, or [Bytes.length bytes] of it *)
          let to_read = min !chunk_size (Bytes.length slice.bytes) in
          read_exactly_
            ~too_short:(fun () -> raise (fail "chunk is too short"))
            ic slice.bytes to_read;
          slice.len <- to_read;
          chunk_size := !chunk_size - to_read
        )

      method close () = eof := true (* do not close underlying stream *)
    end

  (** Output a stream using chunked encoding *)
  let output_chunked' ?buf (oc : #Iostream.Out_buf.t) (self : #t) : unit =
    let oc' = Output.chunk_encoding ?buf oc ~close_rec:false in
    match to_chan' oc' self with
    | () -> Output.close oc'
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Output.close oc';
      Printexc.raise_with_backtrace e bt

  (** print a stream as a series of chunks *)
  let output_chunked ?buf (oc : out_channel) (self : #t) : unit =
    output_chunked' ?buf (Output.of_out_channel oc) self
end

(** A writer abstraction. *)
module Writer = struct
  type t = { write: Output.t -> unit } [@@unboxed]
  (** Writer.

    A writer is a push-based stream of bytes.
    Give it an output channel and it will write the bytes in it.

    This is useful for responses: an http endpoint can return a writer
    as its response's body; the writer is given access to the connection
    to the client and can write into it as if it were a regular
    [out_channel], including controlling calls to [flush].
    Tiny_httpd will convert these writes into valid HTTP chunks.
    @since 0.14
    *)

  let[@inline] make ~write () : t = { write }

  (** Write into the channel. *)
  let[@inline] write (oc : #Output.t) (self : t) : unit =
    self.write (oc :> Output.t)

  (** Empty writer, will output 0 bytes. *)
  let empty : t = { write = ignore }

  (** A writer that just emits the bytes from the given string. *)
  let[@inline] of_string (str : string) : t =
    let write oc = Iostream.Out.output_string oc str in
    { write }

  let[@inline] of_input (ic : #Input.t) : t =
    { write = (fun oc -> Input.to_chan' oc ic) }
end

(** A TCP server abstraction. *)
module TCP_server = struct
  type conn_handler = {
    handle: client_addr:Unix.sockaddr -> Input.t -> Output.t -> unit;
        (** Handle client connection *)
  }

  type t = {
    endpoint: unit -> string * int;
        (** Endpoint we listen on. This can only be called from within [serve]. *)
    active_connections: unit -> int;
        (** Number of connections currently active *)
    running: unit -> bool;  (** Is the server currently running? *)
    stop: unit -> unit;
        (** Ask the server to stop. This might not take effect immediately,
      and is idempotent. After this [server.running()] must return [false]. *)
  }
  (** A running TCP server.

     This contains some functions that provide information about the running
     server, including whether it's active (as opposed to stopped), a function
     to stop it, and statistics about the number of connections. *)

  type builder = {
    serve: after_init:(t -> unit) -> handle:conn_handler -> unit -> unit;
        (** Blocking call to listen for incoming connections and handle them.
            Uses the connection handler [handle] to handle individual client
            connections in individual threads/fibers/tasks.
            @param after_init is called once with the server after the server
            has started. *)
  }
  (** A TCP server builder implementation.

      Calling [builder.serve ~after_init ~handle ()] starts a new TCP server on
      an unspecified endpoint
      (most likely coming from the function returning this builder)
      and returns the running server. *)
end
