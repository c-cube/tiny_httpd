module Buf = Buf
include Server
module Util = Util
module Dir = Tiny_httpd_unix.Dir

module type VFS = Tiny_httpd_unix.Dir.VFS

module Html = Tiny_httpd_html
module IO = Tiny_httpd_core.IO
module Pool = Tiny_httpd_core.Pool
module Log = Tiny_httpd_core.Log
module Server = Tiny_httpd_core.Server
include Server

open struct
  let get_max_connection_ ?(max_connections = 64) () : int =
    let max_connections = max 4 max_connections in
    max_connections

  let clear_slice (slice : IO.Slice.t) =
    Bytes.fill slice.bytes 0 (Bytes.length slice.bytes) '\x00';
    slice.off <- 0;
    slice.len <- 0
end

let create ?(masksigpipe = true) ?max_connections ?(timeout = 0.0) ?buf_size
    ?(get_time_s = Unix.gettimeofday)
    ?(new_thread = fun f -> ignore (Thread.create f () : Thread.t))
    ?(addr = "127.0.0.1") ?(port = 8080) ?sock ?middlewares () : t =
  let max_connections = get_max_connection_ ?max_connections () in
  let server =
    {
      Tiny_httpd_unix.Unix_tcp_server_.addr;
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
      running = true;
      port;
      sock;
      max_connections;
      sem_max_connections = Tiny_httpd_unix.Sem.create max_connections;
      masksigpipe;
      timeout;
    }
  in
  let tcp_server_builder =
    Tiny_httpd_unix.Unix_tcp_server_.to_tcp_server server
  in
  let module B = struct
    let init_addr () = addr
    let init_port () = port
    let get_time_s = get_time_s
    let tcp_server () = tcp_server_builder
  end in
  let backend = (module B : IO_BACKEND) in
  Server.create_from ?buf_size ?middlewares ~backend ()
