open Common_ws_

module With_lock = struct
  type t = { with_lock: 'a. (unit -> 'a) -> 'a }
  type builder = unit -> t

  let default_builder : builder =
   fun () ->
    let mutex = Mutex.create () in
    {
      with_lock =
        (fun f ->
          Mutex.lock mutex;
          try
            let x = f () in
            Mutex.unlock mutex;
            x
          with e ->
            Mutex.unlock mutex;
            raise e);
    }
end

type handler = unit Request.t -> IO.Input.t -> IO.Output.t -> unit

module Frame_type = struct
  type t = int

  let continuation : t = 0
  let text : t = 1
  let binary : t = 2
  let close : t = 8
  let ping : t = 9
  let pong : t = 10

  let show = function
    | 0 -> "continuation"
    | 1 -> "text"
    | 2 -> "binary"
    | 8 -> "close"
    | 9 -> "ping"
    | 10 -> "pong"
    | _ty -> spf "unknown frame type %xd" _ty
end

module Header = struct
  type t = {
    mutable fin: bool;
    mutable ty: Frame_type.t;
    mutable payload_len: int;
    mutable mask: bool;
    mask_key: bytes;  (** len = 4 *)
  }

  let create () : t =
    {
      fin = false;
      ty = 0;
      payload_len = 0;
      mask = false;
      mask_key = Bytes.create 4;
    }
end

exception Close_connection
(** Raised to close the connection. *)

module Writer = struct
  type t = {
    header: Header.t;
    header_buf: bytes;
    buf: bytes;  (** bufferize writes *)
    mutable offset: int;  (** number of bytes already in [buf] *)
    oc: IO.Output.t;
    mutable closed: bool;
    mutex: With_lock.t;
  }

  let create ?(buf_size = 16 * 1024) ~with_lock ~oc () : t =
    {
      header = Header.create ();
      header_buf = Bytes.create 16;
      buf = Bytes.create buf_size;
      offset = 0;
      oc;
      closed = false;
      mutex = with_lock;
    }

  let[@inline] close self = self.closed <- true
  let int_of_bool : bool -> int = Obj.magic

  (** Write the frame header to [self.oc] *)
  let write_header_ (self : t) : unit =
    let header_len = ref 2 in
    let b0 =
      Char.chr ((int_of_bool self.header.fin lsl 7) lor self.header.ty)
    in
    Bytes.unsafe_set self.header_buf 0 b0;

    (* we don't mask *)
    let payload_len = self.header.payload_len in
    let payload_first_byte =
      if payload_len < 126 then
        payload_len
      else if payload_len < 1 lsl 16 then (
        Bytes.set_int16_be self.header_buf 2 payload_len;
        header_len := 4;
        126
      ) else (
        Bytes.set_int64_be self.header_buf 2 (Int64.of_int payload_len);
        header_len := 10;
        127
      )
    in

    let b1 =
      Char.chr @@ ((int_of_bool self.header.mask lsl 7) lor payload_first_byte)
    in
    Bytes.unsafe_set self.header_buf 1 b1;

    if self.header.mask then (
      Bytes.blit self.header_buf !header_len self.header.mask_key 0 4;
      header_len := !header_len + 4
    );

    (*Log.debug (fun k ->
        k "websocket: write header ty=%s (%d B)"
          (Frame_type.show self.header.ty)
          !header_len);*)
    IO.Output.output self.oc self.header_buf 0 !header_len;
    ()

  (** Max fragment size: send 16 kB at a time *)
  let max_fragment_size = 16 * 1024

  let[@inline never] really_output_buf_ (self : t) =
    self.header.fin <- true;
    self.header.ty <- Frame_type.binary;
    self.header.payload_len <- self.offset;
    self.header.mask <- false;
    write_header_ self;

    IO.Output.output self.oc self.buf 0 self.offset;
    IO.Output.flush self.oc;
    self.offset <- 0

  let flush_ (self : t) =
    if self.closed then raise Close_connection;
    if self.offset > 0 then really_output_buf_ self

  let[@inline] flush_if_full (self : t) : unit =
    if self.offset = Bytes.length self.buf then really_output_buf_ self

  let send_pong (self : t) : unit =
    let@ () = self.mutex.with_lock in
    self.header.fin <- true;
    self.header.ty <- Frame_type.pong;
    self.header.payload_len <- 0;
    self.header.mask <- false;
    (* only write a header, we don't send a payload at all *)
    write_header_ self

  let output_char (self : t) c : unit =
    let@ () = self.mutex.with_lock in
    let cap = Bytes.length self.buf - self.offset in
    (* make room for [c] *)
    if cap = 0 then really_output_buf_ self;
    Bytes.set self.buf self.offset c;
    self.offset <- self.offset + 1;
    (* if [c] made the buffer full, then flush it *)
    if cap = 1 then really_output_buf_ self

  let output (self : t) buf i len : unit =
    let@ () = self.mutex.with_lock in
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      flush_if_full self;

      let n = min !len (Bytes.length self.buf - self.offset) in
      assert (n > 0);

      Bytes.blit buf !i self.buf self.offset n;
      self.offset <- self.offset + n;

      i := !i + n;
      len := !len - n
    done;
    flush_if_full self

  let flush self : unit =
    let@ () = self.mutex.with_lock in
    flush_ self
end

module Reader = struct
  type state =
    | Begin  (** At the beginning of a frame *)
    | Reading_frame of { mutable remaining_bytes: int; mutable num_read: int }
        (** Currently reading the payload of a frame with [remaining_bytes] left
            to read from the underlying [ic] *)
    | Close

  type t = {
    ic: IO.Input.t;
    writer: Writer.t;  (** Writer, to send "pong" *)
    header_buf: bytes;  (** small buffer to read frame headers *)
    small_buf: bytes;  (** Used for control frames *)
    header: Header.t;
    last_ty: Frame_type.t;  (** Last frame's type, used for continuation *)
    mutable state: state;
  }

  let create ~ic ~(writer : Writer.t) () : t =
    {
      ic;
      header_buf = Bytes.create 8;
      small_buf = Bytes.create 128;
      writer;
      state = Begin;
      last_ty = 0;
      header = Header.create ();
    }

  (** limitation: we only accept frames that are 2^30 bytes long or less *)
  let max_fragment_size = 1 lsl 30

  (** Read next frame header into [self.header] *)
  let read_frame_header (self : t) : unit =
    (* read header *)
    IO.Input.really_input self.ic self.header_buf 0 2;

    let b0 = Bytes.unsafe_get self.header_buf 0 |> Char.code in
    let b1 = Bytes.unsafe_get self.header_buf 1 |> Char.code in

    self.header.fin <- b0 land 1 == 1;
    let ext = (b0 lsr 4) land 0b0111 in
    if ext <> 0 then (
      Log.error (fun k -> k "websocket: unknown extension %d, closing" ext);
      raise Close_connection
    );

    self.header.ty <- b0 land 0b0000_1111;
    self.header.mask <- b1 land 0b1000_0000 != 0;

    let payload_len : int =
      let len = b1 land 0b0111_1111 in
      if len = 126 then (
        IO.Input.really_input self.ic self.header_buf 0 2;
        Bytes.get_uint16_be self.header_buf 0
      ) else if len = 127 then (
        IO.Input.really_input self.ic self.header_buf 0 8;
        let len64 = Bytes.get_int64_be self.header_buf 0 in
        if Int64.compare len64 (Int64.of_int max_fragment_size) > 0 then (
          Log.error (fun k ->
              k "websocket: maximum frame fragment exceeded (%Ld > %d)" len64
                max_fragment_size);
          raise Close_connection
        );

        Int64.to_int len64
      ) else
        len
    in
    self.header.payload_len <- payload_len;

    if self.header.mask then
      IO.Input.really_input self.ic self.header.mask_key 0 4;

    (*Log.debug (fun k ->
        k "websocket: read frame header type=%s payload_len=%d mask=%b"
          (Frame_type.show self.header.ty)
          self.header.payload_len self.header.mask);*)
    ()

  external apply_masking_ :
    key:bytes -> key_offset:int -> buf:bytes -> int -> int -> unit
    = "tiny_httpd_ws_apply_masking"
  [@@noalloc]
  (** Apply masking to the parsed data *)

  let[@inline] apply_masking ~mask_key ~mask_offset (buf : bytes) off len : unit
      =
    assert (
      Bytes.length mask_key = 4
      && mask_offset >= 0 && off >= 0
      && off + len <= Bytes.length buf);
    apply_masking_ ~key:mask_key ~key_offset:mask_offset ~buf off len

  let read_body_to_string (self : t) : string =
    let len = self.header.payload_len in
    let buf = Bytes.create len in
    IO.Input.really_input self.ic buf 0 len;
    if self.header.mask then
      apply_masking ~mask_key:self.header.mask_key ~mask_offset:0 buf 0 len;
    Bytes.unsafe_to_string buf

  (** Skip bytes of the body *)
  let skip_body (self : t) : unit =
    let len = ref self.header.payload_len in
    while !len > 0 do
      let n = min !len (Bytes.length self.small_buf) in
      IO.Input.really_input self.ic self.small_buf 0 n;
      len := !len - n
    done

  (** State machine that reads [len] bytes into [buf] *)
  let rec read_rec (self : t) buf i len : int =
    match self.state with
    | Close -> 0
    | Reading_frame r when r.remaining_bytes = 0 ->
      self.state <- Begin;
      read_rec self buf i len
    | Reading_frame r ->
      let len = min len r.remaining_bytes in
      let n = IO.Input.input self.ic buf i len in

      (* apply masking *)
      if self.header.mask then
        apply_masking ~mask_key:self.header.mask_key ~mask_offset:r.num_read buf
          i n
      else (
        Log.error (fun k -> k "websocket: client's frames must be masked");
        raise Close_connection
      );

      (* update state *)
      r.remaining_bytes <- r.remaining_bytes - n;
      r.num_read <- r.num_read + n;
      if r.remaining_bytes = 0 then self.state <- Begin;
      n
    | Begin ->
      read_frame_header self;
      Log.debug (fun k ->
          k "websocket: read frame of type=%s payload_len=%d key=%S"
            (Frame_type.show self.header.ty)
            self.header.payload_len
            (Bytes.unsafe_to_string self.header.mask_key));

      (match self.header.ty with
      | 0 ->
        (* continuation *)
        if self.last_ty = 1 || self.last_ty = 2 then
          self.state <-
            Reading_frame
              { remaining_bytes = self.header.payload_len; num_read = 0 }
        else (
          Log.error (fun k ->
              k "continuation frame coming after frame of type %s"
                (Frame_type.show self.last_ty));
          raise Close_connection
        );
        read_rec self buf i len
      | 1 ->
        (* text *)
        self.state <-
          Reading_frame
            { remaining_bytes = self.header.payload_len; num_read = 0 };
        read_rec self buf i len
      | 2 ->
        (* binary *)
        self.state <-
          Reading_frame
            { remaining_bytes = self.header.payload_len; num_read = 0 };
        read_rec self buf i len
      | 8 ->
        (* close frame *)
        self.state <- Close;
        let body = read_body_to_string self in
        if String.length body >= 2 then (
          let errcode = Bytes.get_int16_be (Bytes.unsafe_of_string body) 0 in
          Log.info (fun k ->
              k "client send 'close' with errcode=%d, message=%S" errcode
                (String.sub body 2 (String.length body - 2)))
        );
        0
      | 9 ->
        (* ping, reply *)
        skip_body self;
        Writer.send_pong self.writer;
        read_rec self buf i len
      | 10 ->
        (* pong, just ignore *)
        skip_body self;
        read_rec self buf i len
      | ty ->
        Log.error (fun k -> k "unknown frame type: %xd" ty);
        raise Close_connection)

  let read self buf i len =
    try read_rec self buf i len
    with Close_connection ->
      self.state <- Close;
      0

  let close self : unit =
    if self.state != Close then (
      Log.debug (fun k -> k "websocket: close connection from server side");
      self.state <- Close
    )
end

let upgrade ?(with_lock = With_lock.default_builder ()) ic oc : _ * _ =
  let writer = Writer.create ~with_lock ~oc () in
  let reader = Reader.create ~ic ~writer () in
  let ws_ic : IO.Input.t =
    object
      inherit IO.Input.t_from_refill ~bytes:(Bytes.create 4_096) ()

      method private refill (slice : IO.Slice.t) =
        slice.off <- 0;
        slice.len <- Reader.read reader slice.bytes 0 (Bytes.length slice.bytes)

      method close () = Reader.close reader
    end
  in
  let ws_oc : IO.Output.t =
    object
      method close () = Writer.close writer
      method flush () = Writer.flush writer
      method output bs i len = Writer.output writer bs i len
      method output_char c = Writer.output_char writer c
    end
  in
  ws_ic, ws_oc

(** Turn a regular connection handler (provided by the user) into a websocket
    upgrade handler *)
module Make_upgrade_handler (X : sig
  val accept_ws_protocol : string -> bool
  val with_lock : With_lock.builder
  val handler : handler
end) : Server.UPGRADE_HANDLER with type handshake_state = unit Request.t =
struct
  type handshake_state = unit Request.t

  let name = "websocket"

  open struct
    exception Bad_req of string

    let bad_req msg = raise (Bad_req msg)
    let bad_reqf fmt = Printf.ksprintf bad_req fmt
  end

  let handshake_ (req : unit Request.t) =
    (match Request.get_header req "sec-websocket-protocol" with
    | None -> ()
    | Some proto when not (X.accept_ws_protocol proto) ->
      bad_reqf "handler rejected websocket protocol %S" proto
    | Some _proto -> ());
    let key =
      match Request.get_header req "sec-websocket-key" with
      | None -> bad_req "need sec-websocket-key"
      | Some k -> k
    in

    (* TODO: "origin" header *)

    (* produce the accept key *)
    let accept =
      (* yes, SHA1 is broken. It's also part of the spec for websockets. *)
      Utils_.sha_1 (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
      |> Utils_.B64.encode ~url:false
    in

    let headers = [ "sec-websocket-accept", accept ] in
    Log.debug (fun k ->
        k "websocket: upgrade successful, accept key is %S" accept);
    headers, req

  let handshake _addr req : _ result =
    try Ok (handshake_ req) with Bad_req s -> Error s

  let handle_connection req ic oc =
    let with_lock = X.with_lock () in
    let ws_ic, ws_oc = upgrade ~with_lock ic oc in
    try X.handler req ws_ic ws_oc
    with Close_connection ->
      Log.debug (fun k -> k "websocket: requested to close the connection");
      ()
end

let add_route_handler ?accept ?(accept_ws_protocol = fun _ -> true) ?middlewares
    ?(with_lock = With_lock.default_builder) (server : Server.t) route
    (f : handler) : unit =
  let module M = Make_upgrade_handler (struct
    let handler = f
    let with_lock = with_lock
    let accept_ws_protocol = accept_ws_protocol
  end) in
  let up : Server.upgrade_handler = (module M) in
  Server.add_upgrade_handler ?accept ?middlewares server route up

module Private_ = struct
  let apply_masking = Reader.apply_masking
end
