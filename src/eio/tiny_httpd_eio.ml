module IO = Tiny_httpd_io
module H = Tiny_httpd_server
module Pool = Tiny_httpd_pool

let ( let@ ) = ( @@ )

type 'a with_args =
  ?addr:string ->
  ?port:int ->
  ?max_connections:int ->
  stdenv:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  'a

let get_max_connection_ ?(max_connections = 64) () : int =
  let max_connections = max 4 max_connections in
  max_connections

let buf_size = 16 * 1024

let ic_of_flow ~buf_pool:ic_pool (flow : Eio.Net.stream_socket) :
    IO.In_channel.t =
  Pool.with_resource ic_pool @@ fun cstruct ->
  let len_slice = ref 0 in
  let offset = ref 0 in

  let input buf i len =
    if len = 0 then
      0
    else (
      let available = ref (!len_slice - !offset) in
      if !available = 0 then (
        let n = flow#read_into cstruct in
        offset := 0;
        len_slice := n;
        available := n
      );

      let n = min !available len in
      if n > 0 then (
        Cstruct.blit_to_bytes cstruct !offset buf i n;
        offset := !offset + n;
        n
      ) else
        0
    )
  in
  let close () = flow#shutdown `Receive in
  { IO.In_channel.input; close }

let oc_of_flow ~buf_pool:oc_pool (flow : Eio.Net.stream_socket) :
    IO.Out_channel.t =
  (* write buffer *)
  Pool.with_resource oc_pool @@ fun wbuf ->
  let offset = ref 0 in

  let flush () =
    if !offset > 0 then (
      let i = ref 0 in
      let len = ref !offset in

      let src =
        object
          inherit Eio.Flow.source

          method read_into (cstruct : Cstruct.t) : int =
            if !len = 0 then raise End_of_file;
            let n = min !len (Cstruct.length cstruct) in
            Cstruct.blit_from_bytes wbuf !i cstruct 0 n;
            i := !i + n;
            len := !len - n;
            n
        end
      in

      flow#copy src;
      offset := 0
    )
  in

  let output buf i len =
    let i = ref i in
    let len = ref len in

    while !len > 0 do
      let available = Bytes.length wbuf - !offset in
      let n = min !len available in
      Bytes.blit buf !i wbuf !offset n;
      offset := !offset + n;
      i := !i + n;
      len := !len - n;

      if !offset = Bytes.length wbuf then flush ()
    done
  in

  let output_char c =
    if !offset = Bytes.length wbuf then flush ();
    Bytes.set wbuf !offset c;
    incr offset;
    if !offset = Bytes.length wbuf then flush ()
  in

  let close () = flow#shutdown `Send in
  { IO.Out_channel.close; flush; output; output_char }

let io_backend ?(addr = "127.0.0.1") ?(port = 8080) ?max_connections
    ~(stdenv : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) () :
    (module H.IO_BACKEND) =
  let module M = struct
    let init_addr () = addr
    let init_port () = port
    let get_time_s () = Unix.gettimeofday ()
    let ic_pool = Pool.create ~mk_item:(fun () -> Cstruct.create buf_size) ()
    let oc_pool = Pool.create ~mk_item:(fun () -> Bytes.create buf_size) ()

    let tcp_server () : IO.TCP_server.builder =
      {
        IO.TCP_server.serve =
          (fun ~after_init ~handle () : unit ->
            (* FIXME: parse *)
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
                stop =
                  (fun () ->
                    Atomic.set running false;
                    Eio.Switch.fail sw Exit);
                endpoint =
                  (fun () ->
                    (* TODO: find the real port *)
                    addr, port);
                active_connections = (fun () -> Atomic.get active_conns);
              }
            in

            after_init tcp_server;

            while Atomic.get running do
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
                  let ic = ic_of_flow ~buf_pool:ic_pool flow in
                  let oc = oc_of_flow ~buf_pool:oc_pool flow in
                  handle.handle ic oc)
            done);
      }
  end in
  (module M)

let create ?addr ?port ?max_connections ~stdenv ~sw ?buf_size ?middlewares () :
    H.t =
  let backend = io_backend ?addr ?port ?max_connections ~stdenv ~sw () in
  H.create_from ?buf_size ?middlewares ~backend ()
