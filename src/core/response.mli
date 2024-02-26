(** Responses

    Responses are what a http server, such as {!Tiny_httpd}, send back to
    the client to answer a {!Request.t}*)

type body =
  [ `String of string | `Stream of IO.Input.t | `Writer of IO.Writer.t | `Void ]
(** Body of a response, either as a simple string,
      or a stream of bytes, or nothing (for server-sent events notably).

      - [`String str] replies with a body set to this string, and a known content-length.
      - [`Stream str] replies with a body made from this string, using chunked encoding.
      - [`Void] replies with no body.
      - [`Writer w] replies with a body created by the writer [w], using
          a chunked encoding.
        It is available since 0.14.
  *)

type t = private {
  code: Response_code.t;  (** HTTP response code. See {!Response_code}. *)
  headers: Headers.t;
      (** Headers of the reply. Some will be set by [Tiny_httpd] automatically. *)
  body: body;  (** Body of the response. Can be empty. *)
}
(** A response to send back to a client. *)

val set_body : body -> t -> t
(** Set the body of the response.
      @since 0.11 *)

val set_header : string -> string -> t -> t
(** Set a header.
      @since 0.11 *)

val update_headers : (Headers.t -> Headers.t) -> t -> t
(** Modify headers.
      @since 0.11 *)

val remove_header : string -> t -> t
(** Remove one instance of this header.
      @since NEXT_RELEASE *)

val set_headers : Headers.t -> t -> t
(** Set all headers.
      @since 0.11 *)

val set_code : Response_code.t -> t -> t
(** Set the response code.
      @since 0.11 *)

val make_raw : ?headers:Headers.t -> code:Response_code.t -> string -> t
(** Make a response from its raw components, with a string body.
      Use [""] to not send a body at all. *)

val make_raw_stream :
  ?headers:Headers.t -> code:Response_code.t -> IO.Input.t -> t
(** Same as {!make_raw} but with a stream body. The body will be sent with
      the chunked transfer-encoding. *)

val make_void : ?headers:Headers.t -> code:int -> unit -> t
(** Return a response without a body at all.
      @since 0.13 *)

val make :
  ?headers:Headers.t ->
  ?code:int ->
  (body, Response_code.t * string) result ->
  t
(** [make r] turns a result into a response.

      - [make (Ok body)] replies with [200] and the body.
      - [make (Error (code,msg))] replies with the given error code
        and message as body.
  *)

val make_string :
  ?headers:Headers.t ->
  ?code:int ->
  (string, Response_code.t * string) result ->
  t
(** Same as {!make} but with a string body. *)

val make_writer :
  ?headers:Headers.t ->
  ?code:int ->
  (IO.Writer.t, Response_code.t * string) result ->
  t
(** Same as {!make} but with a writer body. *)

val make_stream :
  ?headers:Headers.t ->
  ?code:int ->
  (IO.Input.t, Response_code.t * string) result ->
  t
(** Same as {!make} but with a stream body. *)

val fail : ?headers:Headers.t -> code:int -> ('a, unit, string, t) format4 -> 'a
(** Make the current request fail with the given code and message.
      Example: [fail ~code:404 "oh noes, %s not found" "waldo"].
  *)

val fail_raise : code:int -> ('a, unit, string, 'b) format4 -> 'a
(** Similar to {!fail} but raises an exception that exits the current handler.
      This should not be used outside of a (path) handler.
      Example: [fail_raise ~code:404 "oh noes, %s not found" "waldo"; never_executed()]
  *)

val pp : Format.formatter -> t -> unit
(** Pretty print the response. The exact format is not specified. *)

(**/**)

module Private_ : sig
  val make_void_force_ : ?headers:Headers.t -> code:int -> unit -> t
  val output_ : bytes:Bytes.t -> IO.Output.t -> t -> unit
end

(**/**)
