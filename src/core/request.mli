(** Requests

    Requests are sent by a client, e.g. a web browser or cURL.
    From the point of view of the server, they're inputs. *)

open Common_

type 'body t = private {
  meth: Meth.t;  (** HTTP method for this request. *)
  host: string;
      (** Host header, mandatory. It can also be found in {!headers}. *)
  client_addr: Unix.sockaddr;  (** Client address. Available since 0.14. *)
  headers: Headers.t;  (** List of headers. *)
  mutable meta: Hmap.t;  (** Metadata. @since NEXT_RELEASE *)
  http_version: int * int;
      (** HTTP version. This should be either [1, 0] or [1, 1]. *)
  path: string;  (** Full path of the requested URL. *)
  path_components: string list;
      (** Components of the path of the requested URL. *)
  query: (string * string) list;  (** Query part of the requested URL. *)
  body: 'body;  (** Body of the request. *)
  start_time: float;
      (** Obtained via [get_time_s] in {!create}
        @since 0.11 *)
}
(** A request with method, path, host, headers, and a body, sent by a client.

      The body is polymorphic because the request goes through
      several transformations. First it has no body, as only the request
      and headers are read; then it has a stream body; then the body might be
      entirely read as a string via {!read_body_full}.

      @since 0.6 The field [query] was added and contains the query parameters in ["?foo=bar,x=y"]
      @since 0.6 The field [path_components] is the part of the path that precedes [query] and is split on ["/"].
      @since 0.11 the type is a private alias
      @since 0.11 the field [start_time] was added
  *)

val add_meta : _ t -> 'a Hmap.key -> 'a -> unit
(** Add metadata
 @since NEXT_RELEASE *)

val get_meta : _ t -> 'a Hmap.key -> 'a option
(** Get metadata
 @since NEXT_RELEASE *)

val get_meta_exn : _ t -> 'a Hmap.key -> 'a
(** Like {!get_meta} but can fail
    @raise Invalid_argument if not present
 @since NEXT_RELEASE *)

val pp_with :
  ?mask_header:(string -> bool) ->
  ?headers_to_mask:string list ->
  ?show_query:bool ->
  ?pp_body:(Format.formatter -> 'body -> unit) ->
  unit ->
  Format.formatter ->
  'body t ->
  unit
(** Pretty print the request. The exact format of this printing
    is not specified.
    @param mask_header function which is given each header name. If it
      returns [true], the header's value is masked. The presence of
      the header is still printed. Default [fun _ -> false].
    @param headers_to_mask a list of headers masked by default.
      Default is ["authorization"; "cookie"].
    @show_query if [true] (default [true]), the query part of the
      request is shown.
    @param pp_body body printer (default prints "?" instead of the body,
      which works even for stream bodies) *)

val pp : Format.formatter -> string t -> unit
(** Pretty print the request and its body. The exact format of this printing
      is not specified. *)

val pp_ : Format.formatter -> _ t -> unit
(** Pretty print the request without its body. The exact format of this printing
      is not specified. *)

val headers : _ t -> Headers.t
(** List of headers of the request, including ["Host"]. *)

val get_header : ?f:(string -> string) -> _ t -> string -> string option
(** [get_header req h] looks up header [h] in [req]. It returns [None] if the
      header is not present. This is case insensitive and should be used
      rather than looking up [h] verbatim in [headers]. *)

val get_header_int : _ t -> string -> int option
(** Same as {!get_header} but also performs a string to integer conversion. *)

val set_header : string -> string -> 'a t -> 'a t
(** [set_header k v req] sets [k: v] in the request [req]'s headers. *)

val remove_header : string -> 'a t -> 'a t
(** Remove one instance of this header.
      @since NEXT_RELEASE *)

val update_headers : (Headers.t -> Headers.t) -> 'a t -> 'a t
(** Modify headers using the given function.
      @since 0.11 *)

val set_body : 'a -> _ t -> 'a t
(** [set_body b req] returns a new query whose body is [b].
      @since 0.11 *)

val host : _ t -> string
(** Host field of the request. It also appears in the headers. *)

val client_addr : _ t -> Unix.sockaddr
(** Client address of the request.
      @since 0.16 *)

val meth : _ t -> Meth.t
(** Method for the request. *)

val path : _ t -> string
(** Request path. *)

val query : _ t -> (string * string) list
(** Decode the query part of the {!path} field.
      @since 0.4 *)

val body : 'b t -> 'b
(** Request body, possibly empty. *)

val start_time : _ t -> float
(** time stamp (from {!Unix.gettimeofday}) after parsing the first line of the request
      @since 0.11 *)

val limit_body_size :
  max_size:int -> bytes:bytes -> IO.Input.t t -> IO.Input.t t
(** Limit the body size to [max_size] bytes, or return
      a [413] error.
      @since 0.3
  *)

val read_body_full : ?bytes:bytes -> ?buf_size:int -> IO.Input.t t -> string t
(** Read the whole body into a string. Potentially blocking.

    @param buf_size initial size of underlying buffer (since 0.11)
    @param bytes the initial buffer (since 0.14)
    *)

(**/**)

(* for internal usage, do not use.  There is no guarantee of stability. *)
module Private_ : sig
  val parse_req_start :
    client_addr:Unix.sockaddr ->
    get_time_s:(unit -> float) ->
    buf:Buf.t ->
    IO.Input.t ->
    unit t option resp_result

  val parse_req_start_exn :
    ?buf:Buf.t ->
    client_addr:Unix.sockaddr ->
    get_time_s:(unit -> float) ->
    IO.Input.t ->
    unit t option

  val close_after_req : _ t -> bool
  val parse_body : ?bytes:bytes -> unit t -> IO.Input.t -> IO.Input.t t
  val set_body : 'a -> _ t -> 'a t
end

(**/**)
