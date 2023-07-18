(** IO abstraction.

    We abstract IO so we can support classic unix blocking IOs
    with threads, and modern async IO with Eio.

    {b NOTE}: experimental.

    @since NEXT_RELEASE
*)

module Buf = Tiny_httpd_buf

(** Input channel (byte source) *)
module In_channel = struct
  type t = {
    input: bytes -> int -> int -> int;
        (** Read into the slice. Returns [0] only if the
        channel is closed. *)
    close: unit -> unit;  (** Close the input. Must be idempotent. *)
  }
  (** An input channel, i.e an incoming stream of bytes.

      This can be a [string], an [int_channel], an [Unix.file_descr], a
      decompression wrapper around another input channel, etc. *)

  let of_in_channel ?(close_noerr = false) (ic : in_channel) : t =
    {
      input = (fun buf i len -> input ic buf i len);
      close =
        (fun () ->
          if close_noerr then
            close_in_noerr ic
          else
            close_in ic);
    }

  let of_unix_fd ?(close_noerr = false) (fd : Unix.file_descr) : t =
    {
      input = (fun buf i len -> Unix.read fd buf i len);
      close =
        (fun () ->
          if close_noerr then (
            try Unix.close fd with _ -> ()
          ) else
            Unix.close fd);
    }

  (** Read into the given slice.
      @return the number of bytes read, [0] means end of input. *)
  let[@inline] input (self : t) buf i len = self.input buf i len

  (** Close the channel. *)
  let[@inline] close self : unit = self.close ()
end

(** Output channel (byte sink) *)
module Out_channel = struct
  type t = {
    output: bytes -> int -> int -> unit;  (** Output slice *)
    flush: unit -> unit;  (** Flush underlying buffer *)
    close: unit -> unit;  (** Close the output. Must be idempotent. *)
  }
  (** An output channel, ie. a place into which we can write bytes.

      This can be a [Buffer.t], an [out_channel], a [Unix.file_descr], etc. *)

  (** [of_out_channel oc] wraps the channel into a {!Out_channel.t}.
      @param close_noerr if true, then closing the result uses [close_out_noerr]
      instead of [close_out] to close [oc] *)
  let of_out_channel ?(close_noerr = false) (oc : out_channel) : t =
    {
      output = (fun buf i len -> output oc buf i len);
      flush = (fun () -> flush oc);
      close =
        (fun () ->
          if close_noerr then
            close_out_noerr oc
          else
            close_out oc);
    }

  (** [of_buffer buf] is an output channel that writes directly into [buf].
        [flush] and [close] have no effect. *)
  let of_buffer (buf : Buffer.t) : t =
    { output = Buffer.add_subbytes buf; flush = ignore; close = ignore }

  (** Output the buffer slice into this channel *)
  let[@inline] output (self : t) buf i len : unit = self.output buf i len

  let[@inline] output_string (self : t) (str : string) : unit =
    self.output (Bytes.unsafe_of_string str) 0 (String.length str)

  (** Close the channel. *)
  let[@inline] close self : unit = self.close ()

  (** Flush (ie. force write) any buffered bytes. *)
  let[@inline] flush self : unit = self.flush ()

  let output_buf (self : t) (buf : Buf.t) : unit =
    let b = Buf.bytes_slice buf in
    output self b 0 (Buf.size buf)

  (** [chunk_encoding oc] makes a new channel that outputs its content into [oc]
      in chunk encoding form.
      @param close_rec if true, closing the result will also close [oc]
      *)
  let chunk_encoding ~close_rec (self : t) : t =
    let flush = self.flush in
    let close () =
      (* write an empty chunk to close the stream *)
      output_string self "0\r\n";
      (* write another crlf after the stream (see #56) *)
      output_string self "\r\n";
      self.flush ();
      if close_rec then self.close ()
    in
    let output buf i n =
      if n > 0 then (
        output_string self (Printf.sprintf "%x\r\n" n);
        self.output buf i n;
        output_string self "\r\n"
      )
    in
    { flush; close; output }
end

(** A writer abstraction. *)
module Writer = struct
  type t = { write: Out_channel.t -> unit } [@@unboxed]
  (** Writer.

    A writer is a push-based stream of bytes.
    Give it an output channel and it will write the bytes in it.

    This is useful for responses: an http endpoint can return a writer
    as its response's body, and output into it as if it were a regular
    [out_channel], including controlling calls to [flush].
    @since NEXT_RELEASE
    *)

  let[@inline] make ~write () : t = { write }

  (** Write into the channel. *)
  let[@inline] write (oc : Out_channel.t) (self : t) : unit = self.write oc

  (** Empty writer, will output 0 bytes. *)
  let empty : t = { write = ignore }

  (** A writer that just emits the bytes from the given string. *)
  let[@inline] of_string (str : string) : t =
    let write oc = Out_channel.output_string oc str in
    { write }
end

(** A TCP server abstraction. *)
module TCP_server = struct
  type conn_handler = {
    handle: In_channel.t -> Out_channel.t -> unit;
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
