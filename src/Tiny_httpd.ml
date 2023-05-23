(** Tiny Httpd.

    A small HTTP/1.1 server, in pure OCaml, along with some utilities
    to build small websites. The primary use case is to build UIs for tools
    that are {b not} primarily websites, but can benefit from an embedded
    web server.
*)

module Buf = Tiny_httpd_buf
module Byte_stream = Tiny_httpd_stream
include Tiny_httpd_server
module Util = Tiny_httpd_util
module Dir = Tiny_httpd_dir
module Html = Tiny_httpd_html
