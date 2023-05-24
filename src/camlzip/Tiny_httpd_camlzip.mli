(** Middleware for compression.

    This uses camlzip to provide deflate compression/decompression.
    If installed, the middleware will compress responses' bodies
    when they are streams or fixed-size above a given limit
    (but it will not compress small, fixed-size bodies).
  *)

val middleware :
  ?compress_above:int -> ?buf_size:int -> unit -> Tiny_httpd_server.Middleware.t
(** Middleware responsible for deflate compression/decompression.
    @param compress_above threshold, in bytes, above which a response body
      that has a known content-length is compressed. Stream bodies
      are always compressed.
    @param buf_size size of the underlying buffer for compression/decompression
    @since 0.11 *)

val setup : ?compress_above:int -> ?buf_size:int -> Tiny_httpd_server.t -> unit
(** Install middleware for tiny_httpd to be able to encode/decode
    compressed streams
    @param compress_above threshold above with string responses are compressed
    @param buf_size size of the underlying buffer for compression/decompression *)
