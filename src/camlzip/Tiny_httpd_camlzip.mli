
val setup : ?buf_size:int -> Tiny_httpd.t -> unit
(** Install callbacks for tiny_httpd to be able to encode/decode
    compressed streams
    @param buf_size size of the underlying buffer for compression/decompression *)
