module IO = Tiny_httpd.IO
module H = Tiny_httpd.Server
module Pool = Tiny_httpd.Pool
module Slice = IO.Slice
module Log = Tiny_httpd.Log
module Task = Task

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

type 'a with_args =
  ?addr:string ->
  ?port:int ->
  ?unix_sock:string ->
  ?max_connections:int ->
  ?max_buf_pool_size:int ->
  ?buf_size:int ->
  'a

let get_max_connection_ ?(max_connections = 64) () : int =
  let max_connections = max 4 max_connections in
  max_connections

let default_buf_size = 16 * 1024

let show_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let ic_of_fd ~(num_open : int ref) ~bytes (fd : Lwt_unix.file_descr) :
    IO.Input.t =
  object
    inherit Iostream.In_buf.t_from_refill ~bytes ()

    method private refill (sl : Slice.t) =
      assert (sl.len = 0);
      sl.off <- 0;
      let n =
        Lwt_unix.read fd sl.bytes 0 (Bytes.length sl.bytes) |> Task.await
      in
      sl.len <- n

    method close () =
      decr num_open;
      if !num_open <= 0 then Lwt_unix.close fd |> Task.await
  end

let oc_of_fd ~(num_open : int ref) ~bytes (fd : Lwt_unix.file_descr) :
    IO.Output.t =
  object
    inherit IO.Output.t_from_output ~bytes ()
    (* method flush () : unit = Lwt_io.flush oc |> Task.await *)

    method private output_underlying buf i len =
      let i = ref i in
      let len = ref len in
      while !len > 0 do
        let n = Lwt_unix.write fd buf !i !len |> Task.await in
        i := !i + n;
        len := !len - n
      done

    method private close_underlying () =
      decr num_open;
      if !num_open <= 0 then Lwt_unix.close fd |> Task.await
  end

let io_backend ?addr ?port ?unix_sock ?max_connections ?max_buf_pool_size
    ?(buf_size = default_buf_size) () : (module H.IO_BACKEND) =
  let _buf_pool =
    Pool.create ?max_size:max_buf_pool_size
      ~mk_item:(fun () -> Lwt_bytes.create buf_size)
      ()
  in

  let addr, port, (sockaddr : Unix.sockaddr) =
    match addr, port, unix_sock with
    | _, _, Some s -> Printf.sprintf "unix:%s" s, 0, Unix.ADDR_UNIX s
    | addr, port, None ->
      let addr = Option.value ~default:"127.0.0.1" addr in
      let sockaddr, port =
        match Lwt_unix.getaddrinfo addr "" [] |> Task.await, port with
        | { Unix.ai_addr = ADDR_INET (h, _); _ } :: _, None ->
          let p = 8080 in
          Unix.ADDR_INET (h, p), p
        | { Unix.ai_addr = ADDR_INET (h, _); _ } :: _, Some p ->
          Unix.ADDR_INET (h, p), p
        | _ ->
          failwith @@ Printf.sprintf "Could not parse TCP address from %S" addr
      in
      addr, port, sockaddr
  in

  let module M = struct
    let init_addr () = addr
    let init_port () = port
    let get_time_s () = Unix.gettimeofday ()
    let max_connections = get_max_connection_ ?max_connections ()

    let pool_size =
      match max_buf_pool_size with
      | Some n -> n
      | None -> min 4096 (max_connections * 2)

    let tcp_server () : IO.TCP_server.builder =
      {
        IO.TCP_server.serve =
          (fun ~after_init ~handle () : unit ->
            let server_done, set_server_done = Lwt.wait () in
            let running = Atomic.make true in
            let active_conns = Atomic.make 0 in

            (* Eio.Switch.on_release sw (fun () -> Atomic.set running false); *)
            let port = ref port in

            let server_loop : unit Lwt.t =
              let@ () = Task.run in
              let backlog = max_connections in
              let sock =
                Lwt_unix.socket ~cloexec:true
                  (Unix.domain_of_sockaddr sockaddr)
                  Unix.SOCK_STREAM 0
              in
              Lwt_unix.setsockopt sock Unix.TCP_NODELAY true;
              Lwt_unix.setsockopt_optint sock Unix.SO_LINGER None;
              Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
              Lwt_unix.setsockopt sock Unix.SO_REUSEPORT true;
              Lwt_unix.bind sock sockaddr |> Task.await;
              Lwt_unix.listen sock backlog;

              (* recover real port, if any *)
              (match Unix.getsockname (Lwt_unix.unix_file_descr sock) with
              | Unix.ADDR_INET (_, p) -> port := p
              | _ -> ());

              let handle_client client_addr fd : unit =
                Atomic.incr active_conns;
                let@ () = Task.run_async in

                let cleanup () =
                  Log.debug (fun k ->
                      k "Tiny_httpd_lwt: client handler returned");
                  Atomic.decr active_conns
                in

                let buf_ic = Bytes.create buf_size in
                let buf_oc = Bytes.create buf_size in
                (*
                let@ buf_ic = Pool.with_resource buf_pool in
                let@ buf_oc = Pool.with_resource buf_pool in
*)

                (* close FD when both ends are closed *)
                let num_open = ref 2 in
                let ic = ic_of_fd ~num_open ~bytes:buf_ic fd in
                let oc = oc_of_fd ~num_open ~bytes:buf_oc fd in
                try
                  handle.handle ~client_addr ic oc;
                  cleanup ()
                with exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  cleanup ();
                  Log.error (fun k ->
                      k "Client handler for %s failed with %s\n%s"
                        (show_sockaddr client_addr)
                        (Printexc.to_string exn)
                        (Printexc.raw_backtrace_to_string bt))
              in

              while Atomic.get running do
                let fd, addr = Lwt_unix.accept sock |> Task.await in
                handle_client addr fd
              done
            in

            let tcp_server : IO.TCP_server.t =
              {
                running = (fun () -> Atomic.get running);
                stop =
                  (fun () ->
                    Atomic.set running false;
                    Lwt.wakeup_later set_server_done ();
                    Task.await server_loop);
                endpoint = (fun () -> addr, !port);
                active_connections = (fun () -> Atomic.get active_conns);
              }
            in

            after_init tcp_server;
            Task.await server_done);
      }
  end in
  (module M)

let create ?addr ?port ?unix_sock ?max_connections ?max_buf_pool_size ?buf_size
    ?middlewares () : H.t Lwt.t =
  let@ () = Task.run in
  let backend =
    io_backend ?addr ?port ?unix_sock ?max_buf_pool_size ?max_connections
      ?buf_size ()
  in
  H.create_from ?buf_size ?middlewares ~backend ()
