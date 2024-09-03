include Tiny_httpd
module Fd = Io_server.Fd

open struct
  let get_max_connection_ ?(max_connections = 64) () : int =
    let max_connections = max 4 max_connections in
    max_connections

  let clear_slice (slice : IO.Slice.t) =
    Bytes.fill slice.bytes 0 (Bytes.length slice.bytes) '\x00';
    slice.off <- 0;
    slice.len <- 0
end

let create ?max_connections ?(timeout = 0.0) ?buf_size
    ?(get_time_s = Unix.gettimeofday) ?(addr = "127.0.0.1") ?(port = 8080)
    ?(sock : Fd.t option) ?middlewares ~(runner : Moonpool.Runner.t) () : t =
  let new_thread f =
    ignore (Moonpool_fib.spawn_top ~on:runner f : _ Moonpool_fib.t)
  in
  let max_connections = get_max_connection_ ?max_connections () in
  let server =
    {
      Io_server.addr;
      new_thread;
      buf_pool =
        Pool.create ~clear:Buf.clear_and_zero
          ~mk_item:(fun () -> Buf.create ?size:buf_size ())
          ();
      slice_pool =
        Pool.create ~clear:clear_slice
          ~mk_item:
            (let buf_size = Option.value buf_size ~default:4096 in
             fun () -> IO.Slice.create buf_size)
          ();
      running = Atomic.make true;
      port;
      sock;
      max_connections;
      sem_max_connections = Io_server.Sem.make max_connections;
      timeout;
    }
  in
  let tcp_server_builder = Io_server.to_tcp_server server in
  let module B = struct
    let init_addr () = addr
    let init_port () = port
    let get_time_s = get_time_s
    let tcp_server () = tcp_server_builder
  end in
  let backend = (module B : IO_BACKEND) in
  Server.create_from ?buf_size ?middlewares ~backend ()
