(** IO abstraction.

    We abstract IO so we can support classic unix blocking IOs
    with threads, and modern async IO with Eio.

    {b NOTE}: experimental.

    @since NEXT_RELEASE
*)

module In_channel = struct
  type t = {
    input: bytes -> int -> int -> int;
        (** Read into the slice. Returns [0] only if the
        channel is closed. *)
    close: unit -> unit;
  }

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
end

(** A TCP server abstraction *)
module TCP_server = struct
  type conn_handler = {
    handle: In_channel.t -> Out_channel.t -> unit;
        (** Handle client connection *)
  }

  type t = {
    listen: handle:conn_handler -> unit -> unit;
        (** Blocking call to start listening for incoming connections.
            Uses the connection handler to handle individual client connections. *)
    endpoint: unit -> Unix.inet_addr * int;  (** Endpoint we listen on *)
  }
end
