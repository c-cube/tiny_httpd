module F = Fuseau_unix
module IO = Tiny_httpd_io
module Pool = Tiny_httpd_pool
module Buf = Tiny_httpd_buf

let ( let@ ) = ( @@ )

module Server = struct
  type t = {
    addr: Unix.inet_addr;
    port: int;
    server_sock: Unix.file_descr;
    buf_pool: Buf.t Pool.t;
    mutable active: bool;
    mutable n_connections: int;
  }

  let create ~buf_pool ~addr ~port () : t =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.setsockopt sock Unix.SO_REUSEPORT true;
    Unix.bind sock (Unix.ADDR_INET (addr, port));
    Unix.set_nonblock sock;
    Unix.listen sock 32;
    let self =
      {
        addr;
        port;
        server_sock = sock;
        active = true;
        n_connections = 0;
        buf_pool;
      }
    in
    self

  let ic_of_fd ~(buf : Buf.t) ~close fd : IO.Input.t =
    let buf = Buf.bytes_slice buf in
    let buf_i = ref 0 in
    let buf_len = ref 0 in
    let eof = ref false in

    let refill () =
      if not !eof then (
        buf_i := 0;
        buf_len := F.IO_unix.read fd buf 0 (Bytes.length buf);
        if !buf_len = 0 then eof := true
      )
    in

    let input bs i len =
      if !buf_len = 0 then refill ();
      let n = min len !buf_len in
      Bytes.blit buf !buf_i bs i n;
      buf_i := !buf_i + n;
      buf_len := !buf_len - n;
      n
    in

    { input; close }

  let oc_of_fd ~buf ~close fd : IO.Output.t =
    let buf = Buf.bytes_slice buf in
    let off = ref 0 in

    let flush () =
      if !off > 0 then (
        F.IO_unix.write fd buf 0 !off;
        off := 0
      )
    in
    let[@inline] maybe_flush () = if !off = Bytes.length buf then flush () in

    let output_char c =
      maybe_flush ();
      Bytes.set buf !off c;
      incr off
    in

    let output bs i len =
      let i = ref i in
      let len = ref len in
      while !len > 0 do
        maybe_flush ();
        let n = min !len (Bytes.length buf - !off) in
        Bytes.blit bs !i buf !off n;
        off := !off + n;
        i := !i + n;
        len := !len - n
      done;
      maybe_flush ()
    in
    { output; output_char; flush; close }

  type conn_handler = Tiny_httpd_io.TCP_server.conn_handler

  let loop_client self ~(handler : conn_handler) client_sock client_addr : unit
      =
    Unix.set_nonblock client_sock;
    Unix.setsockopt client_sock Unix.TCP_NODELAY true;

    (* idempotent close *)
    let closed = ref false in
    let close () =
      if not !closed then (
        closed := true;
        Unix.shutdown client_sock Unix.SHUTDOWN_ALL;
        Unix.close client_sock
      )
    in

    let@ buf_ic = Pool.with_resource self.buf_pool in
    let@ buf_oc = Pool.with_resource self.buf_pool in
    let ic = ic_of_fd ~buf:buf_ic ~close client_sock in
    let oc = oc_of_fd ~buf:buf_oc ~close client_sock in
    let finally () =
      self.n_connections <- self.n_connections - 1;
      close ()
    in
    let@ () = Fun.protect ~finally in
    handler.handle ~client_addr ic oc

  let loop (self : t) ~(handler : Tiny_httpd_io.TCP_server.conn_handler) : unit
      =
    while self.active do
      match Unix.accept self.server_sock with
      | client_sock, client_addr ->
        self.n_connections <- 1 + self.n_connections;
        ignore
          (Fuseau.spawn ~propagate_cancel_to_parent:false (fun () ->
               loop_client self ~handler client_sock client_addr)
            : unit F.Fiber.t)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (* FIXME: possible race condition: the socket became readable
            in the mid-time and we won't get notified. We need to call
            [accept] after subscribing to [on_readable]. *)
        F.IO_unix.await_readable self.server_sock
    done

  let close self =
    if self.active then (
      self.active <- false;
      try Unix.close self.server_sock with _ -> ()
    )
end

let io_backend ~buf_pool ?(addr = Unix.inet_addr_loopback) ~port () :
    (module Tiny_httpd_server.IO_BACKEND) =
  let module M = struct
    let init_addr () = Unix.string_of_inet_addr addr
    let init_port () = port

    let get_time_s () =
      let t_ns = Mtime_clock.now () |> Mtime.to_uint64_ns in
      Int64.to_float t_ns *. 1e-9

    let serve ~after_init ~(handle : Tiny_httpd_io.TCP_server.conn_handler) () :
        unit =
      let server = Server.create ~buf_pool ~addr ~port () in
      let server' : Tiny_httpd_io.TCP_server.t =
        {
          endpoint = (fun () -> Unix.string_of_inet_addr addr, port);
          active_connections = (fun () -> server.n_connections);
          running = (fun () -> server.active);
          stop = (fun () -> server.active <- false);
        }
      in

      after_init server';
      Server.loop server ~handler:handle;
      ()

    let tcp_server () : Tiny_httpd_io.TCP_server.builder = { serve }
  end in
  (module M)
