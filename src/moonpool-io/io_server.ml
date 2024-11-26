open Tiny_httpd_core
module A = Atomic
module MIO = Moonpool_io
module Sem = Moonpool_sync.Semaphore.Counting
module Fd = Moonpool_io.Fd

module IO_helper = struct
  module Slice = Iostream.Slice

  module Output = struct
    include IO.Output

    class of_unix_fd ?(close_noerr = false) ~closed ~(buf : Slice.t) (fd : Fd.t) :
      t =
      object
        inherit t_from_output ~bytes:buf.bytes ()

        method private output_underlying bs i len0 =
          let i = ref i in
          let len = ref len0 in
          while !len > 0 do
            match MIO.Unix.write fd bs !i !len with
            | 0 -> failwith "write failed"
            | n ->
              i := !i + n;
              len := !len - n
          done

        method private close_underlying () =
          if not !closed then (
            closed := true;
            if close_noerr then (
              try MIO.Unix.close fd with _ -> ()
            ) else
              MIO.Unix.close fd
          )
      end
  end

  module Input = struct
    include IO.Input

    let of_unix_fd ?(close_noerr = false) ~closed ~(buf : Slice.t) (fd : Fd.t) :
        t =
      let eof = ref false in
      object
        inherit Iostream.In_buf.t_from_refill ~bytes:buf.bytes ()

        method private refill (slice : Slice.t) =
          if not !eof then (
            slice.off <- 0;
            let continue = ref true in
            while !continue do
              match
                MIO.Unix.read fd slice.bytes 0 (Bytes.length slice.bytes)
              with
              | n ->
                slice.len <- n;
                continue := false
            done;
            (* Printf.eprintf "read returned %d B\n%!" !n; *)
            if slice.len = 0 then eof := true
          )

        method close () =
          if not !closed then (
            closed := true;
            eof := true;
            if close_noerr then (
              try MIO.Unix.close fd with _ -> ()
            ) else
              MIO.Unix.close fd
          )
      end
  end
end

open struct
  let get_addr_ (fd : Fd.t) =
    match Unix.getsockname (Fd.unsafe_get fd) with
    | Unix.ADDR_INET (addr, port) -> addr, port
    | _ -> invalid_arg "httpd: address is not INET"

  let shutdown_silent_ (fd : Fd.t) : unit =
    try MIO.Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ()

  let close_silent_ (fd : Fd.t) : unit = try MIO.Unix.close fd with _ -> ()
end

type t = {
  addr: string;
  port: int;
  buf_pool: Buf.t Pool.t;
  slice_pool: IO.Slice.t Pool.t;
  max_connections: int;
  sem_max_connections: Sem.t;
      (** semaphore to restrict the number of active concurrent connections *)
  mutable sock: Fd.t option;  (** Socket *)
  new_thread: (unit -> unit) -> unit;
  timeout: float;
  running: bool A.t; (* TODO: use an atomic? *)
}

let to_tcp_server (self : t) : IO.TCP_server.builder =
  {
    IO.TCP_server.serve =
      (fun ~after_init ~handle () : unit ->
        let sock, should_bind =
          match self.sock with
          | Some s ->
            (* Because we're getting a socket from the caller (e.g. systemd) *)
            s, false
          | None ->
            let sock =
              Unix.socket
                (if Util.is_ipv6_str self.addr then
                  Unix.PF_INET6
                else
                  Unix.PF_INET)
                Unix.SOCK_STREAM 0
            in
            let fd = Fd.create sock in
            fd, true (* Because we're creating the socket ourselves *)
        in
        MIO.Unix.set_nonblock sock;
        MIO.Unix.setsockopt_optint sock Unix.SO_LINGER None;
        if should_bind then (
          let inet_addr = Unix.inet_addr_of_string self.addr in
          MIO.Unix.setsockopt sock Unix.SO_REUSEADDR true;
          MIO.Unix.bind sock (Unix.ADDR_INET (inet_addr, self.port));
          let n_listen = 2 * self.max_connections in
          MIO.Unix.listen sock n_listen
        );

        self.sock <- Some sock;

        let tcp_server =
          {
            IO.TCP_server.stop = (fun () -> Atomic.set self.running false);
            running = (fun () -> Atomic.get self.running);
            active_connections =
              (fun () ->
                self.max_connections - Sem.get_value self.sem_max_connections);
            endpoint =
              (fun () ->
                let addr, port = get_addr_ sock in
                Unix.string_of_inet_addr addr, port);
          }
        in
        after_init tcp_server;

        (* how to handle a single client *)
        let handle_client_ (client_sock : Fd.t) (client_addr : Unix.sockaddr) :
            unit =
          Log.debug (fun k ->
              k "t[%d]: serving new client on %s"
                (Thread.id @@ Thread.self ())
                (Util.show_sockaddr client_addr));

          MIO.Unix.set_nonblock client_sock;
          MIO.Unix.setsockopt client_sock Unix.TCP_NODELAY true;
          MIO.Unix.(setsockopt_float client_sock SO_RCVTIMEO self.timeout);
          MIO.Unix.(setsockopt_float client_sock SO_SNDTIMEO self.timeout);

          Pool.with_resource self.slice_pool @@ fun ic_buf ->
          Pool.with_resource self.slice_pool @@ fun oc_buf ->
          let closed = ref false in

          let oc =
            new IO_helper.Output.of_unix_fd
              ~close_noerr:true ~closed ~buf:oc_buf client_sock
          in
          let ic =
            IO_helper.Input.of_unix_fd ~close_noerr:true ~closed ~buf:ic_buf
              client_sock
          in
          handle.handle ~client_addr ic oc
        in

        MIO.Unix.set_nonblock sock;
        while Atomic.get self.running do
          match MIO.Unix.accept sock with
          | client_sock, client_addr ->
            (* limit concurrency *)
            Sem.acquire self.sem_max_connections;
            self.new_thread (fun () ->
                try
                  handle_client_ client_sock client_addr;
                  Log.debug (fun k ->
                      k "t[%d]: done with client on %s, exiting"
                        (Thread.id @@ Thread.self ())
                      @@ Util.show_sockaddr client_addr);
                  shutdown_silent_ client_sock;
                  close_silent_ client_sock;
                  Sem.release self.sem_max_connections
                with e ->
                  let bt = Printexc.get_raw_backtrace () in
                  shutdown_silent_ client_sock;
                  close_silent_ client_sock;
                  Sem.release self.sem_max_connections;
                  Log.error (fun k ->
                      k
                        "@[<v>Handler: uncaught exception for client %s:@ %s@ \
                         %s@]"
                        (Util.show_sockaddr client_addr)
                        (Printexc.to_string e)
                        (Printexc.raw_backtrace_to_string bt)))
          | exception e ->
            Log.error (fun k ->
                k "Unix.accept raised an exception: %s" (Printexc.to_string e));
            Atomic.set self.running false
        done;

        (* Wait for all threads to be done: this only works if all threads are done. *)
        MIO.Unix.close sock;
        while Sem.get_value self.sem_max_connections < self.max_connections do
          Sem.acquire self.sem_max_connections
        done;
        ());
  }
