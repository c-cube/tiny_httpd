(** IO abstraction.

    We abstract IO so we can support classic unix blocking IOs
    with threads, and modern async IO with Eio.

    {b NOTE}: experimental.

    @since NEXT_RELEASE
*)

module Buf = Tiny_httpd_buf

module In_channel = struct
  type t = {
    input: bytes -> int -> int -> int;
        (** Read into the slice. Returns [0] only if the
        channel is closed. *)
    close: unit -> unit;
  }
  (** An input channel, i.e an incoming stream of bytes. *)

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

  let[@inline] input (self : t) buf i len = self.input buf i len
  let[@inline] close self : unit = self.close ()
end

module Out_channel = struct
  type t = {
    output: bytes -> int -> int -> unit;  (** Output slice *)
    flush: unit -> unit;  (** Flush underlying buffer *)
    close: unit -> unit;
  }
  (** An output channel, ie. a place into which we can write bytes. *)

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

  let[@inline] output (self : t) buf i len : unit = self.output buf i len

  let[@inline] output_string (self : t) (str : string) : unit =
    self.output (Bytes.unsafe_of_string str) 0 (String.length str)

  let[@inline] close self : unit = self.close ()
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

(** A writer abstraction.

    A writer is a push-based stream of bytes. Give it an output channel and it will write
    the bytes in it. *)
module Writer = struct
  type t = { write: Out_channel.t -> unit } [@@unboxed]
  (** Writer. *)

  let[@inline] make ~write () : t = { write }

  (** Write into the channel. *)
  let[@inline] write (oc : Out_channel.t) (self : t) : unit = self.write oc

  let empty : t = { write = ignore }

  (** A writer that just emits the bytes from the given string. *)
  let of_string (str : string) : t =
    let write oc = Out_channel.output_string oc str in
    { write }
end

(** A TCP server abstraction *)
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
        (** Ask the server to stop. This might not take effect immediately. *)
  }
  (** Running server. *)

  type builder = {
    serve: after_init:(t -> unit) -> handle:conn_handler -> unit -> unit;
        (** Blocking call to listen for incoming connections and handle them.
            Uses the connection handler to handle individual client connections. *)
  }
  (** A TCP server implementation. *)
end
