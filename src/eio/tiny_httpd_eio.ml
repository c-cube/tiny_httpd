module IO = Tiny_httpd.IO
module H = Tiny_httpd.Server
module Pool = Tiny_httpd.Pool
module Slice = IO.Slice
module Log = Tiny_httpd.Log

let ( let@ ) = ( @@ )

type 'a with_args =
  ?addr:string ->
  ?port:int ->
  ?unix_sock:string ->
  ?max_connections:int ->
  ?max_buf_pool_size:int ->
  stdenv:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  'a

let get_max_connection_ ?(max_connections = 64) () : int =
  let max_connections = max 4 max_connections in
  max_connections

let buf_size = 16 * 1024

let eio_ipaddr_to_unix (a : _ Eio.Net.Ipaddr.t) : Unix.inet_addr =
  (* TODO: for ipv4 we really could do it faster via sprintf 🙄 *)
  Unix.inet_addr_of_string (Format.asprintf "%a" Eio.Net.Ipaddr.pp a)

let eio_sock_addr_to_unix (a : Eio.Net.Sockaddr.stream) : Unix.sockaddr =
  match a with
  | `Tcp (h, p) -> Unix.ADDR_INET (eio_ipaddr_to_unix h, p)
  | `Unix s -> Unix.ADDR_UNIX s

let ic_of_flow ~closed ~buf_pool:ic_pool (flow : _ Eio.Net.stream_socket) :
    IO.Input.t =
  let cstruct = Pool.Raw.acquire ic_pool in
  let sent_shutdown = ref false in

  object
    inherit Iostream.In_buf.t_from_refill ()

    method private refill (sl : Slice.t) =
      assert (sl.len = 0);
      let cap = min (Bytes.length sl.bytes) (Cstruct.length cstruct) in

      match Eio.Flow.single_read flow (Cstruct.sub cstruct 0 cap) with
      | exception End_of_file ->
        Log.debug (fun k -> k "read: eof");
        ()
      | n ->
        Log.debug (fun k -> k "read %d bytes..." n);
        Cstruct.blit_to_bytes cstruct 0 sl.bytes 0 n;
        sl.off <- 0;
        sl.len <- n

    method close () =
      if not !closed then (
        closed := true;
        Pool.Raw.release ic_pool cstruct
      );
      if not !sent_shutdown then (
        sent_shutdown := true;
        Eio.Flow.shutdown flow `Receive
      )
  end

let oc_of_flow ~closed ~buf_pool:oc_pool (flow : _ Eio.Net.stream_socket) :
    IO.Output.t =
  (* write buffer *)
  let wbuf : Cstruct.t = Pool.Raw.acquire oc_pool in
  let offset = ref 0 in
  let sent_shutdown = ref false in

  object (self)
    method flush () : unit =
      if !offset > 0 then (
        Eio.Flow.write flow [ Cstruct.sub wbuf 0 !offset ];
        offset := 0
      )

    method output buf i len =
      let i = ref i in
      let len = ref len in

      while !len > 0 do
        let available = Cstruct.length wbuf - !offset in
        let n = min !len available in
        Cstruct.blit_from_bytes buf !i wbuf !offset n;
        offset := !offset + n;
        i := !i + n;
        len := !len - n;

        if !offset = Cstruct.length wbuf then self#flush ()
      done

    method output_char c =
      if !offset = Cstruct.length wbuf then self#flush ();
      Cstruct.set_char wbuf !offset c;
      incr offset;
      if !offset = Cstruct.length wbuf then self#flush ()

    method close () =
      if not !closed then (
        closed := true;
        Pool.Raw.release oc_pool wbuf
      );
      if not !sent_shutdown then (
        sent_shutdown := true;
        Eio.Flow.shutdown flow `Send
      )
  end

let io_backend ?addr ?port ?unix_sock ?max_connections ?max_buf_pool_size
    ~(stdenv : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) () :
    (module H.IO_BACKEND) =
  let addr, port, (sockaddr : Eio.Net.Sockaddr.stream) =
    match addr, port, unix_sock with
    | _, _, Some s -> Printf.sprintf "unix:%s" s, 0, `Unix s
    | addr, port, None ->
      let addr = Option.value ~default:"127.0.0.1" addr in
      let sockaddr, port =
        match Eio.Net.getaddrinfo stdenv#net addr, port with
        | `Tcp (h, _) :: _, None ->
          let p = 8080 in
          `Tcp (h, p), p
        | `Tcp (h, _) :: _, Some p -> `Tcp (h, p), p
        | _ ->
          failwith @@ Printf.sprintf "Could not parse TCP address from %S" addr
      in
      addr, port, sockaddr
  in

  let module M = struct
    let init_addr () = addr
    let init_port () = port
    let clock = Eio.Stdenv.clock stdenv
    let get_time_s () = Eio.Time.now clock
    let max_connections = get_max_connection_ ?max_connections ()

    let pool_size =
      match max_buf_pool_size with
      | Some n -> n
      | None -> min 4096 (max_connections * 2)

    let cstruct_pool =
      Pool.create ~max_size:pool_size
        ~mk_item:(fun () -> Cstruct.create buf_size)
        ()

    let tcp_server () : IO.TCP_server.builder =
      {
        IO.TCP_server.serve =
          (fun ~after_init ~handle () : unit ->
            let running = Atomic.make true in
            let active_conns = Atomic.make 0 in
            let sem = Eio.Semaphore.make max_connections in

            Eio.Switch.on_release sw (fun () -> Atomic.set running false);
            let net = Eio.Stdenv.net stdenv in

            (* main server socket *)
            let sock =
              let backlog = max_connections in
              Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw net
                sockaddr
            in

            (* Resolve actual address/port (important for port 0) *)
            let actual_addr, actual_port =
              match Eio.Net.listening_addr sock with
              | `Tcp (_, p) -> addr, p
              | `Unix s -> Printf.sprintf "unix:%s" s, 0
            in

            let tcp_server : IO.TCP_server.t =
              {
                running = (fun () -> Atomic.get running);
                stop =
                  (fun () ->
                    Atomic.set running false;
                    (* Backstop: fail the switch after 60s if handlers don't complete *)
                    Eio.Fiber.fork_daemon ~sw (fun () ->
                        Eio.Time.sleep clock 60.0;
                        if Eio.Switch.get_error sw |> Option.is_none then
                          Eio.Switch.fail sw Exit;
                        `Stop_daemon));
                endpoint = (fun () -> actual_addr, actual_port);
                active_connections = (fun () -> Atomic.get active_conns);
              }
            in

            after_init tcp_server;

            while Atomic.get running do
              match Eio.Net.accept ~sw sock with
              | exception (Eio.Cancel.Cancelled _ | Eio.Io _)
                when not (Atomic.get running) ->
                (* Socket closed or switch cancelled during shutdown; exit loop *)
                ()
              | conn, client_addr ->
                (* Acquire semaphore BEFORE spawning a fiber so we
                   bound the number of in-flight fibers. *)
                Eio.Semaphore.acquire sem;
                Eio.Fiber.fork ~sw (fun () ->
                    let@ () =
                      Fun.protect ~finally:(fun () ->
                          Log.debug (fun k ->
                              k "Tiny_httpd_eio: client handler returned");
                          Atomic.decr active_conns;
                          Eio.Semaphore.release sem;
                          try Eio.Flow.close conn with Eio.Io _ -> ())
                    in
                    (try
                       Eio_unix.Fd.use_exn "setsockopt" (Eio_unix.Net.fd conn)
                         (fun fd -> Unix.setsockopt fd Unix.TCP_NODELAY true)
                     with Unix.Unix_error _ -> ());
                    Atomic.incr active_conns;
                    let ic_closed = ref false in
                    let oc_closed = ref false in
                    let ic =
                      ic_of_flow ~closed:ic_closed ~buf_pool:cstruct_pool conn
                    in
                    let oc =
                      oc_of_flow ~closed:oc_closed ~buf_pool:cstruct_pool conn
                    in

                    Log.debug (fun k ->
                        k "handling client on %a…" Eio.Net.Sockaddr.pp
                          client_addr);
                    let client_addr_unix = eio_sock_addr_to_unix client_addr in
                    try handle.handle ~client_addr:client_addr_unix ic oc
                    with exn ->
                      let bt = Printexc.get_raw_backtrace () in
                      Log.error (fun k ->
                          k "Client handler for %a failed with %s\n%s"
                            Eio.Net.Sockaddr.pp client_addr
                            (Printexc.to_string exn)
                            (Printexc.raw_backtrace_to_string bt)))
            done);
      }
  end in
  (module M)

let create ?addr ?port ?unix_sock ?max_connections ?max_buf_pool_size ~stdenv
    ~sw ?buf_size ?middlewares () : H.t =
  let backend =
    io_backend ?addr ?port ?unix_sock ?max_buf_pool_size ?max_connections
      ~stdenv ~sw ()
  in
  H.create_from ?buf_size ?middlewares ~backend ()
