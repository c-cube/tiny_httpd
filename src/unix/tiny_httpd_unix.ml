module Dir = Dir
module Sem = Sem

module Unix_tcp_server_ = struct
  let get_addr_ sock =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (addr, port) -> addr, port
    | _ -> invalid_arg "httpd: address is not INET"

  type t = {
    addr: string;
    port: int;
    buf_pool: Buf.t Pool.t;
    slice_pool: IO.Slice.t Pool.t;
    max_connections: int;
    sem_max_connections: Sem.t;
        (** semaphore to restrict the number of active concurrent connections *)
    mutable sock: Unix.file_descr option;  (** Socket *)
    new_thread: (unit -> unit) -> unit;
    timeout: float;
    masksigpipe: bool;
    mutable running: bool; (* TODO: use an atomic? *)
  }

  let shutdown_silent_ fd =
    try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ()

  let close_silent_ fd = try Unix.close fd with _ -> ()

  let to_tcp_server (self : t) : IO.TCP_server.builder =
    {
      IO.TCP_server.serve =
        (fun ~after_init ~handle () : unit ->
          if self.masksigpipe && not Sys.win32 then
            ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ] : _ list);
          let sock, should_bind =
            match self.sock with
            | Some s ->
              ( s,
                false
                (* Because we're getting a socket from the caller (e.g. systemd) *)
              )
            | None ->
              ( Unix.socket
                  (if Util.is_ipv6_str self.addr then
                     Unix.PF_INET6
                   else
                     Unix.PF_INET)
                  Unix.SOCK_STREAM 0,
                true (* Because we're creating the socket ourselves *) )
          in
          Unix.clear_nonblock sock;
          Unix.setsockopt_optint sock Unix.SO_LINGER None;
          if should_bind then (
            let inet_addr = Unix.inet_addr_of_string self.addr in
            Unix.setsockopt sock Unix.SO_REUSEADDR true;
            Unix.bind sock (Unix.ADDR_INET (inet_addr, self.port));
            let n_listen = 2 * self.max_connections in
            Unix.listen sock n_listen
          );

          self.sock <- Some sock;

          let tcp_server =
            {
              IO.TCP_server.stop = (fun () -> self.running <- false);
              running = (fun () -> self.running);
              active_connections =
                (fun () -> Sem.num_acquired self.sem_max_connections - 1);
              endpoint =
                (fun () ->
                  let addr, port = get_addr_ sock in
                  Unix.string_of_inet_addr addr, port);
            }
          in
          after_init tcp_server;

          (* how to handle a single client *)
          let handle_client_unix_ (client_sock : Unix.file_descr)
              (client_addr : Unix.sockaddr) : unit =
            Log.debug (fun k ->
                k "t[%d]: serving new client on %s"
                  (Thread.id @@ Thread.self ())
                  (Util.show_sockaddr client_addr));

            if self.masksigpipe && not Sys.win32 then
              ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ] : _ list);
            Unix.set_nonblock client_sock;
            Unix.setsockopt client_sock Unix.TCP_NODELAY true;
            Unix.(setsockopt_float client_sock SO_RCVTIMEO self.timeout);
            Unix.(setsockopt_float client_sock SO_SNDTIMEO self.timeout);

            Pool.with_resource self.slice_pool @@ fun ic_buf ->
            Pool.with_resource self.slice_pool @@ fun oc_buf ->
            let closed = ref false in

            let oc =
              new IO.Output.of_unix_fd
                ~close_noerr:true ~closed ~buf:oc_buf client_sock
            in
            let ic =
              IO.Input.of_unix_fd ~close_noerr:true ~closed ~buf:ic_buf
                client_sock
            in
            handle.handle ~client_addr ic oc
          in

          Unix.set_nonblock sock;
          while self.running do
            match Unix.accept sock with
            | client_sock, client_addr ->
              (* limit concurrency *)
              Sem.acquire 1 self.sem_max_connections;
              (* Block INT/HUP while cloning to avoid children handling them.
                 When thread gets them, our Unix.accept raises neatly. *)
              if not Sys.win32 then
                ignore Unix.(sigprocmask SIG_BLOCK Sys.[ sigint; sighup ]);
              self.new_thread (fun () ->
                  try
                    handle_client_unix_ client_sock client_addr;
                    Log.debug (fun k ->
                        k "t[%d]: done with client on %s, exiting"
                          (Thread.id @@ Thread.self ())
                        @@ Util.show_sockaddr client_addr);
                    shutdown_silent_ client_sock;
                    close_silent_ client_sock;
                    Sem.release 1 self.sem_max_connections
                  with e ->
                    let bt = Printexc.get_raw_backtrace () in
                    shutdown_silent_ client_sock;
                    close_silent_ client_sock;
                    Sem.release 1 self.sem_max_connections;
                    Log.error (fun k ->
                        k
                          "@[<v>Handler: uncaught exception for client %s:@ \
                           %s@ %s@]"
                          (Util.show_sockaddr client_addr)
                          (Printexc.to_string e)
                          (Printexc.raw_backtrace_to_string bt)));
              if not Sys.win32 then
                ignore Unix.(sigprocmask SIG_UNBLOCK Sys.[ sigint; sighup ])
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
              ->
              (* wait for the socket to be ready, and re-enter the loop *)
              ignore (Unix.select [ sock ] [] [ sock ] 1.0 : _ * _ * _)
            | exception e ->
              Log.error (fun k ->
                  k "Unix.accept raised an exception: %s" (Printexc.to_string e));
              Thread.delay 0.01
          done;

          (* Wait for all threads to be done: this only works if all threads are done. *)
          Unix.close sock;
          Sem.acquire self.sem_max_connections.max self.sem_max_connections;
          ());
    }
end
