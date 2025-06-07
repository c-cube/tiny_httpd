open Common_

type resp_error = Response_code.t * string

exception Bad_req = Common_.Bad_req

module Middleware = struct
  type handler = IO.Input.t Request.t -> resp:(Response.t -> unit) -> unit
  type t = handler -> handler

  let[@inline] nil : t = fun h -> h
end

module Head_middleware = struct
  type t = { handle: 'a. 'a Request.t -> 'a Request.t }

  let trivial = { handle = Fun.id }
  let[@inline] apply' req (self : t) = self.handle req

  let to_middleware (self : t) : Middleware.t =
   fun h req ~resp ->
    let req = self.handle req in
    h req ~resp
end

(* a request handler. handles a single request. *)
type cb_path_handler = IO.Output.t -> Middleware.handler

module type SERVER_SENT_GENERATOR = sig
  val set_headers : Headers.t -> unit

  val send_event :
    ?event:string -> ?id:string -> ?retry:string -> data:string -> unit -> unit

  val close : unit -> unit
end

type server_sent_generator = (module SERVER_SENT_GENERATOR)

(** Handler that upgrades to another protocol *)
module type UPGRADE_HANDLER = sig
  type handshake_state
  (** Some specific state returned after handshake *)

  val name : string
  (** Name in the "upgrade" header *)

  val handshake :
    Unix.sockaddr ->
    unit Request.t ->
    (Headers.t * handshake_state, string) result
  (** Perform the handshake and upgrade the connection. The returned code is
      [101] alongside these headers. *)

  val handle_connection : handshake_state -> IO.Input.t -> IO.Output.t -> unit
  (** Take control of the connection and take it from there *)
end

type upgrade_handler = (module UPGRADE_HANDLER)

exception Upgrade of Head_middleware.t list * unit Request.t * upgrade_handler

module type IO_BACKEND = sig
  val init_addr : unit -> string
  val init_port : unit -> int

  val get_time_s : unit -> float
  (** obtain the current timestamp in seconds. *)

  val tcp_server : unit -> IO.TCP_server.builder
  (** Server that can listen on a port and handle clients. *)
end

type handler_result =
  | Handle of (int * Middleware.t) list * cb_path_handler
  | Fail of resp_error
  | Upgrade of Head_middleware.t list * upgrade_handler

let unwrap_handler_result req = function
  | Handle (l, h) -> l, h
  | Fail (c, s) -> raise (Bad_req (c, s))
  | Upgrade (l, up) -> raise (Upgrade (l, req, up))

type t = {
  backend: (module IO_BACKEND);
  enable_logging: bool;
  mutable tcp_server: IO.TCP_server.t option;
  mutable handler: IO.Input.t Request.t -> Response.t;
      (** toplevel handler, if any *)
  mutable head_middlewares: Head_middleware.t list;
  mutable middlewares: (int * Middleware.t) list;  (** Global middlewares *)
  mutable middlewares_sorted: (int * Middleware.t) list lazy_t;
      (** sorted version of {!middlewares} *)
  mutable path_handlers: (unit Request.t -> handler_result option) list;
      (** path handlers *)
  bytes_pool: bytes Pool.t;
}

let addr (self : t) =
  match self.tcp_server with
  | None ->
    let (module B) = self.backend in
    B.init_addr ()
  | Some s -> fst @@ s.endpoint ()

let port (self : t) =
  match self.tcp_server with
  | None ->
    let (module B) = self.backend in
    B.init_port ()
  | Some s -> snd @@ s.endpoint ()

let active_connections (self : t) =
  match self.tcp_server with
  | None -> 0
  | Some s -> s.active_connections ()

let sort_middlewares_ l =
  List.stable_sort (fun (s1, _) (s2, _) -> compare s1 s2) l

let add_middleware ~stage self m =
  let stage =
    match stage with
    | `Encoding -> 0
    | `Stage n when n < 1 -> invalid_arg "add_middleware: bad stage"
    | `Stage n -> n
  in
  self.middlewares <- (stage, m) :: self.middlewares;
  self.middlewares_sorted <- lazy (sort_middlewares_ self.middlewares)

let add_head_middleware (self : t) m : unit =
  self.head_middlewares <- m :: self.head_middlewares

let add_decode_request_cb self f =
  (* turn it into a middleware *)
  let m h req ~resp =
    (* see if [f] modifies the stream *)
    let req0 = Request.Private_.set_body () req in
    match f req0 with
    | None -> h req ~resp (* pass through *)
    | Some (req1, tr_stream) ->
      let body = tr_stream req.Request.body in
      let req = Request.set_body body req1 in
      h req ~resp
  in
  add_middleware self ~stage:`Encoding m

let add_encode_response_cb self f =
  let m h req ~resp =
    h req ~resp:(fun r ->
        let req0 = Request.Private_.set_body () req in
        (* now transform [r] if we want to *)
        match f req0 r with
        | None -> resp r
        | Some r' -> resp r')
  in
  add_middleware self ~stage:`Encoding m

let set_top_handler self f = self.handler <- f

(* route the given handler.
   @param tr_req wraps the actual concrete function returned by the route
   and makes it into a handler. *)
let add_route_handler_ ?(accept = fun _req -> Ok ()) ?(middlewares = []) ?meth
    ~tr_req self (route : _ Route.t) f =
  let middlewares = List.map (fun h -> 5, h) middlewares in
  let ph req : handler_result option =
    match meth with
    | Some m when m <> req.Request.meth -> None (* ignore *)
    | _ ->
      (match Route.Private_.eval req.Request.path_components route f with
      | Some handler ->
        (* we have a handler, do we accept the request based on its headers? *)
        (match accept req with
        | Ok () ->
          Some
            (Handle
               (middlewares, fun oc req ~resp -> tr_req oc req ~resp handler))
        | Error err -> Some (Fail err))
      | None -> None (* path didn't match *))
  in
  self.path_handlers <- ph :: self.path_handlers

let add_route_handler (type a) ?accept ?middlewares ?meth self
    (route : (a, _) Route.t) (f : _) : unit =
  let tr_req _oc req ~resp f =
    let req =
      Pool.with_resource self.bytes_pool @@ fun bytes ->
      Request.read_body_full ~bytes req
    in
    resp (f req)
  in
  add_route_handler_ ?accept ?middlewares ?meth self route ~tr_req f

let add_route_handler_stream ?accept ?middlewares ?meth self route f =
  let tr_req _oc req ~resp f = resp (f req) in
  add_route_handler_ ?accept ?middlewares ?meth self route ~tr_req f

let[@inline] _opt_iter ~f o =
  match o with
  | None -> ()
  | Some x -> f x

exception Exit_SSE

let add_route_server_sent_handler ?accept ?(middlewares = []) self route f =
  let tr_req (oc : IO.Output.t) req ~resp f =
    let req =
      Pool.with_resource self.bytes_pool @@ fun bytes ->
      Request.read_body_full ~bytes req
    in
    let req = List.fold_left Head_middleware.apply' req middlewares in
    let headers =
      ref Headers.(empty |> set "content-type" "text/event-stream")
    in

    (* send response once *)
    let resp_sent = ref false in
    let send_response_idempotent_ () =
      if not !resp_sent then (
        resp_sent := true;
        (* send 200 response now *)
        let initial_resp =
          Response.Private_.make_void_force_ ~headers:!headers ~code:200 ()
        in
        resp initial_resp
      )
    in

    let[@inline] writef fmt =
      Printf.ksprintf (IO.Output.output_string oc) fmt
    in

    let send_event ?event ?id ?retry ~data () : unit =
      send_response_idempotent_ ();
      _opt_iter event ~f:(fun e -> writef "event: %s\n" e);
      _opt_iter id ~f:(fun e -> writef "id: %s\n" e);
      _opt_iter retry ~f:(fun e -> writef "retry: %s\n" e);
      let l = String.split_on_char '\n' data in
      List.iter (fun s -> writef "data: %s\n" s) l;
      IO.Output.output_string oc "\n";
      (* finish group *)
      IO.Output.flush oc
    in
    let module SSG = struct
      let set_headers h =
        if not !resp_sent then (
          headers := List.rev_append h !headers;
          send_response_idempotent_ ()
        )

      let send_event = send_event
      let close () = raise Exit_SSE
    end in
    (try f req (module SSG : SERVER_SENT_GENERATOR)
     with Exit_SSE -> IO.Output.close oc);
    if self.enable_logging then Log.info (fun k -> k "closed SSE connection")
  in
  add_route_handler_ self ?accept ~meth:`GET route ~tr_req f

let add_upgrade_handler ?(accept = fun _ -> Ok ()) ?(middlewares = [])
    (self : t) route f : unit =
  let ph req : handler_result option =
    let middlewares = List.rev_append self.head_middlewares middlewares in
    if req.Request.meth <> `GET then
      None
    else (
      match accept req with
      | Ok () ->
        (match Route.Private_.eval req.Request.path_components route f with
        | Some up -> Some (Upgrade (middlewares, up))
        | None -> None (* path didn't match *))
      | Error err -> Some (Fail err)
    )
  in
  self.path_handlers <- ph :: self.path_handlers

let clear_bytes_ bs = Bytes.fill bs 0 (Bytes.length bs) '\x00'

let create_from ?(enable_logging = not Log.dummy) ?(buf_size = 16 * 1_024)
    ?(head_middlewares = []) ?(middlewares = []) ~backend () : t =
  let handler _req = Response.fail ~code:404 "no top handler" in
  let self =
    {
      backend;
      enable_logging;
      tcp_server = None;
      handler;
      path_handlers = [];
      head_middlewares;
      middlewares = [];
      middlewares_sorted = lazy [];
      bytes_pool =
        Pool.create ~clear:clear_bytes_
          ~mk_item:(fun () -> Bytes.create buf_size)
          ();
    }
  in
  List.iter (fun (stage, m) -> add_middleware self ~stage m) middlewares;
  self

let stop (self : t) =
  match self.tcp_server with
  | None -> ()
  | Some s -> s.stop ()

let running (self : t) =
  match self.tcp_server with
  | None -> false
  | Some s -> s.running ()

let find_map f l =
  let rec aux f = function
    | [] -> None
    | x :: l' ->
      (match f x with
      | Some _ as res -> res
      | None -> aux f l')
  in
  aux f l

let header_list_contains_ (s : string) (name : string) : bool =
  let name' = String.lowercase_ascii name in
  let fragments = String.split_on_char ',' s in
  List.exists
    (fun fragment -> String.lowercase_ascii (String.trim fragment) = name')
    fragments

(** handle client on [ic] and [oc] *)
let client_handle_for (self : t) ~client_addr ic oc : unit =
  Pool.with_resource self.bytes_pool @@ fun bytes_req ->
  Pool.with_resource self.bytes_pool @@ fun bytes_res ->
  let (module B) = self.backend in

  (* how to log the response to this query *)
  let log_response (req : _ Request.t) (resp : Response.t) =
    if self.enable_logging && not Log.dummy then (
      let msgf k =
        let elapsed = B.get_time_s () -. req.start_time in
        k
          ("response to=%s code=%d time=%.3fs meth=%s path=%S" : _ format4)
          (Util.show_sockaddr client_addr)
          resp.code elapsed (Meth.to_string req.meth) req.path
      in
      if Response_code.is_success resp.code then
        Log.info msgf
      else
        Log.error msgf
    )
  in

  let log_exn msg bt =
    Log.error (fun k ->
        k "error while processing response for %s msg=%s@.%s"
          (Util.show_sockaddr client_addr)
          msg
          (Printexc.raw_backtrace_to_string bt))
  in

  (* handle generic exception *)
  let handle_exn e bt : unit =
    let msg = Printexc.to_string e in
    let resp = Response.fail ~code:500 "server error: %s" msg in
    if self.enable_logging && not Log.dummy then log_exn msg bt;
    Response.Private_.output_ ~bytes:bytes_res oc resp
  in

  let handle_bad_req req e bt =
    let msg = Printexc.to_string e in
    let resp = Response.fail ~code:500 "server error: %s" msg in
    if self.enable_logging && not Log.dummy then (
      log_exn msg bt;
      log_response req resp
    );
    Response.Private_.output_ ~bytes:bytes_res oc resp
  in

  let handle_upgrade ~(middlewares : Head_middleware.t list) req
      (module UP : UPGRADE_HANDLER) : unit =
    Log.debug (fun k -> k "upgrade connection");

    let send_resp resp =
      log_response req resp;
      Response.Private_.output_ ~bytes:bytes_res oc resp
    in

    try
      (* apply head middlewares *)
      let req = List.fold_left Head_middleware.apply' req middlewares in

      (* check headers *)
      (match Request.get_header req "connection" with
      | Some str when header_list_contains_ str "Upgrade" -> ()
      | _ -> bad_reqf 426 "connection header must contain 'Upgrade'");
      (match Request.get_header req "upgrade" with
      | Some u when u = UP.name -> ()
      | Some u -> bad_reqf 426 "expected upgrade to be '%s', got '%s'" UP.name u
      | None -> bad_reqf 426 "expected 'connection: upgrade' header");

      (* ok, this is the upgrade we expected *)
      match UP.handshake client_addr req with
      | Error msg ->
        (* fail the upgrade *)
        if self.enable_logging then
          Log.error (fun k -> k "upgrade failed: %s" msg);
        send_resp @@ Response.make_raw ~code:429 "upgrade required"
      | Ok (headers, handshake_st) ->
        (* send the upgrade reply *)
        let headers =
          [ "connection", "upgrade"; "upgrade", UP.name ] @ headers
        in
        send_resp @@ Response.make_string ~code:101 ~headers (Ok "");

        (* handshake successful, proceed with the upgrade handler *)
        UP.handle_connection handshake_st ic oc
    with
    | Bad_req (code, err) -> send_resp @@ Response.make_raw ~code err
    | e ->
      let bt = Printexc.get_raw_backtrace () in
      handle_bad_req req e bt
  in

  let continue = ref true in

  (* merge per-request middlewares with the server-global middlewares *)
  let get_middlewares ~handler_middlewares () : _ list =
    if handler_middlewares = [] then (
      let global_middlewares = Lazy.force self.middlewares_sorted in
      global_middlewares
    ) else
      sort_middlewares_ (List.rev_append handler_middlewares self.middlewares)
  in

  let handle_one_req () =
    match
      let buf = Buf.of_bytes bytes_req in
      Request.Private_.parse_req_start ~client_addr ~get_time_s:B.get_time_s
        ~buf ic
    with
    | Ok None -> continue := false (* client is done *)
    | Error (c, s) ->
      (* connection error, close *)
      let res = Response.make_raw ~code:c s in
      (try Response.Private_.output_ ~bytes:bytes_res oc res
       with Sys_error _ -> ());
      continue := false
    | Ok (Some req) ->
      Log.debug (fun k ->
          k "t[%d]: parsed request: %s"
            (Thread.id @@ Thread.self ())
            (Format.asprintf "@[%a@]" Request.pp_ req));

      if Request.Private_.close_after_req req then continue := false;

      (try
         (* is there a handler for this path? *)
         let handler_middlewares, base_handler =
           match find_map (fun ph -> ph req) self.path_handlers with
           | Some f -> unwrap_handler_result req f
           | None -> [], fun _oc req ~resp -> resp (self.handler req)
         in

         (* handle expect/continue *)
         (match Request.get_header ~f:String.trim req "Expect" with
         | Some "100-continue" ->
           Log.debug (fun k -> k "send back: 100 CONTINUE");
           Response.Private_.output_ ~bytes:bytes_res oc
             (Response.make_raw ~code:100 "")
         | Some s -> bad_reqf 417 "unknown expectation %s" s
         | None -> ());

         let all_middlewares = get_middlewares ~handler_middlewares () in

         (* apply middlewares *)
         let handler oc =
           List.fold_right
             (fun (_, m) h -> m h)
             all_middlewares (base_handler oc)
         in

         (* now actually read request's body into a stream *)
         let req = Request.Private_.parse_body ~bytes:bytes_req req ic in

         (* how to reply *)
         let resp r =
           try
             if Headers.get "connection" r.Response.headers = Some "close" then
               continue := false;
             log_response req r;
             Response.Private_.output_ ~bytes:bytes_res oc r
           with Sys_error e ->
             Log.debug (fun k ->
                 k "error when writing response: %s@.connection broken" e);
             continue := false
         in

         (* call handler *)
         try handler oc req ~resp
         with Sys_error e ->
           Log.debug (fun k ->
               k "error while handling request: %s@.connection broken" e);
           continue := false
       with
      | Sys_error e ->
        (* connection broken somehow *)
        Log.debug (fun k -> k "error: %s@. connection broken" e);
        continue := false
      | Bad_req (code, s) ->
        continue := false;
        let resp = Response.make_raw ~code s in
        log_response req resp;
        Response.Private_.output_ ~bytes:bytes_res oc resp
      | Upgrade _ as e -> raise e
      | e ->
        let bt = Printexc.get_raw_backtrace () in
        handle_bad_req req e bt)
  in

  try
    while !continue && running self do
      Log.debug (fun k ->
          k "t[%d]: read next request" (Thread.id @@ Thread.self ()));
      handle_one_req ()
    done
  with
  | Upgrade (middlewares, req, up) ->
    (* upgrades take over the whole connection, we won't process
       any further request *)
    handle_upgrade ~middlewares req up
  | e ->
    let bt = Printexc.get_raw_backtrace () in
    handle_exn e bt

let client_handler (self : t) : IO.TCP_server.conn_handler =
  { IO.TCP_server.handle = client_handle_for self }

let is_ipv6 (self : t) =
  let (module B) = self.backend in
  Util.is_ipv6_str (B.init_addr ())

let run_exn ?(after_init = ignore) (self : t) : unit =
  let (module B) = self.backend in
  let server = B.tcp_server () in
  server.serve
    ~after_init:(fun tcp_server ->
      self.tcp_server <- Some tcp_server;
      after_init ())
    ~handle:(client_handler self) ()

let run ?after_init self : _ result =
  try Ok (run_exn ?after_init self) with e -> Error e
