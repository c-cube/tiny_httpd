module IO = Tiny_httpd_io
module H = Tiny_httpd_server

let ( let@ ) = ( @@ )

type 'a with_args =
  ?addr:string ->
  ?port:int ->
  ?max_connections:int ->
  stdenv:Eio.Stdenv.t ->
  sw:Eio.Switch.t ->
  'a

let get_max_connection_ ?(max_connections = 64) () : int =
  let max_connections = max 4 max_connections in
  max_connections

let read_buf_size = 4 * 1024
let write_buf_size = 8 * 1024

let ic_of_flow (flow : Eio.Net.stream_socket) : IO.In_channel.t =
  let cstruct = Cstruct.create write_buf_size in
  let input buf i len =
    if len = 0 then
      0
    else (
      let n = flow#read_into (Cstruct.sub cstruct 0 (min len write_buf_size)) in
      Cstruct.blit_to_bytes cstruct 0 buf i n;
      n
    )
  in
  let close () = flow#shutdown `Receive in
  { IO.In_channel.input; close }

let oc_of_flow (flow : Eio.Net.stream_socket) : IO.Out_channel.t =
  let output buf i len =
    if len > 0 then (
      let i = ref i in
      let len = ref len in

      let src =
        object
          inherit Eio.Flow.source

          method read_into (cstruct : Cstruct.t) : int =
            if !len = 0 then raise End_of_file;
            let n = min !len (Cstruct.length cstruct) in
            Cstruct.blit_from_bytes buf !i cstruct 0 n;
            i := !i + n;
            len := !len - n;
            n
        end
      in

      flow#copy src
    )
  in
  let close () = flow#shutdown `Send in
  let flush () = () in
  { IO.Out_channel.close; flush; output }

let io_backend ?(addr = "127.0.0.1") ?(port = 8080) ?max_connections
    ~(stdenv : Eio.Stdenv.t) ~(sw : Eio.Switch.t) () : (module H.IO_BACKEND) =
  let module M = struct
    let init_addr () = addr
    let init_port () = port

    let get_time_s () =
      let clock = Eio.Stdenv.clock stdenv in
      Eio.Time.now clock

    let spawn f : unit =
      Eio.Switch.run @@ fun sub_sw -> Eio.Fiber.fork ~sw:sub_sw f

    let tcp_server () : IO.TCP_server.builder =
      {
        IO.TCP_server.serve =
          (fun ~after_init ~handle () : unit ->
            let ip_addr = Eio.Net.Ipaddr.V4.any in
            let running = Atomic.make true in
            let active_conns = Atomic.make 0 in

            Eio.Switch.on_release sw (fun () -> Atomic.set running false);
            let net = Eio.Stdenv.net stdenv in

            (* main server socket *)
            let sock =
              let backlog = get_max_connection_ ?max_connections () in
              Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw net
                (`Tcp (ip_addr, port))
            in

            let tcp_server : IO.TCP_server.t =
              {
                running = (fun () -> Atomic.get running);
                stop = (fun () -> Atomic.set running false);
                endpoint =
                  (fun () ->
                    (* TODO: find the real port *)
                    addr, port);
                active_connections = (fun () -> Atomic.get active_conns);
              }
            in

            after_init tcp_server;

            while Atomic.get running do
              Eio.Switch.check sw;

              Eio.Net.accept_fork ~sw
                ~on_error:(fun exn ->
                  H._debug (fun k ->
                      k "error in client handler: %s" (Printexc.to_string exn)))
                sock
                (fun flow _client_addr ->
                  Atomic.incr active_conns;
                  let@ () =
                    Fun.protect ~finally:(fun () ->
                        H._debug (fun k ->
                            k "Tiny_httpd_eio: client handler returned");
                        Atomic.decr active_conns)
                  in
                  let ic = ic_of_flow flow in
                  let oc = oc_of_flow flow in
                  handle.handle ic oc)
            done);
      }
  end in
  (module M)

let create ?addr ?port ?max_connections ~stdenv ~sw ?buf_size ?middlewares () :
    H.t =
  let backend = io_backend ?addr ?port ?max_connections ~stdenv ~sw () in
  H.create_from ?buf_size ?middlewares ~backend ()
