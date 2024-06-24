
## 0.17

- add optional middlewares to tiny_httpd_ws
- add `Head_middleware.trivial`
- add `Head_middleware.t`; accept it for SSE/websocket
- add `Request.pp_with` which is a customizable printer
- expose `Response.Bad_req`
- use `iostream` for IOs
- add a `hmap`-typed field to requests, to carry request specific data
    across middlewares
- http_of_dir: ability to setup socket timeout
- add `tiny_httpd.ws`, a websocket library
- add `Response_code.is_success`

- fix: No setting of sigprocmask on Windows
- fix: give the correct code+error if protocol upgrade fails
- remove potentially security-leaking debug line
- fix: avoid collisions in `Mime_` private module
- fix middlewares: merge-sort per-request middleares and global ones
- fix tiny_httpd dir: handle html files

- perf: optim in read_line
- perf: remove some uses of scanf in parsing

- require iostream-camlzip >= 0.2.1
- add optional dependency on `logs`
- logs is a testdep for tiny_httpd_camlzip

## 0.16

- feat: add `Request.client_addr` accessor
- feat: add `tiny_httpd.prometheus`, a simple sub-library
    to expose [prometheus](https://prometheus.io) metrics over HTTP.
- feat: add optional dependency on `logs`

## 0.15

- fix: do not block in `accept`, enabling more graceful shutdown
- improve help message for tiny-httpd-vfs-pack
- security: zero out buffers from pool before reusing them

## 0.14

- breaking: `set_top_handler` takes a stream request, for more generality

- Don't let client handling threads handle SIGINT/SIGHUP
- improve termination behavior (wait for threads to terminate when shutting down server)
- Preserve client address down to Request.t
- add `Tiny_httpd_io` module, abstraction over IOs (output/input) as better IO channels
    than the stdlib's
- add `Tiny_httpd_html.to_writer`
- add `IO.Writer.t`, a push based stream.
- add `Server.run_exn`
- add `Tiny_httpd_pool`
- server: add `IO_BACKEND` abstraction; implement a unix version of it

- perf: use TCP_NODELAY for client sockets
- perf: use a resource pool to recycle buffers, improves memory consumption and GC pressure

## 0.13

- feat: `Server.run` takes `?after_init` parameter
- remove dep on ounit2 and qtest
- expose `Response.make_void`
- Add OPTIONS method
- use ocamlformat on the code

- fix: SSE requires no body
- fix: get addr/port from the current socket
- fix: missing closing crlf in chunked streams
- fix: module Html was not exposed
- fix: close stream after Response.output
- fix(tiny-httpd-vfs-pack): allow redirections when fetching resources

## 0.12

- add dep on `seq`
- add a `Html` module with combinators to produce html
- add `Tiny_httpd_dir.VFS` to emulate file systems
- add a small program, `tiny-httpd-vfs-pack`, to pack directories and files
  (local or behind a URL) into a OCaml module using `VFS`
- show small example of socket activation

## 0.11

- breaking: remove deprecated path handlers based on scanf
- breaking: more getter/setters for request/response, change signatures,
  make request/response private aliases

- fix: release semaphore in case of exception in accept

- feat: add a notion of Middleware
- feat: add `?middlewares` param to `create`
- feat: add `?get_time_s` param to `create`
- feat: close connection if response's headers contains connection
- feat: store `start_time` in request
- feat: implement connection timeout using socket options
  Default is `max_keep_alive = -1.0` which preserves the original behaviour.
- feat: in server-sent-events, add a `close()` function

- refactor(zip): compression is now a middleware
- perf: pass `buf_size` in many places, set default `buf_size` to 16kb
- example: update `echo` to provide a /stats/ endpoint using a middleware

## 0.10

- feat: allow socket activation by passing a raw unix socket to `create`
- fix: `Unix.accept` may raise an exception
  (typicall Unix.EINTR, even with sigpipe blocked ?),
  prevent the server from stopping

## 0.9

- support handlers that stream server-sent events to client

## 0.8

- bump to ocaml 4.04
- Validate header key's character set (#15)
- perf: simpler parsing of headers

- fix: workaround for css/js in `http_of_dir` (#16)
- fix(urlencode): encode non ascii chars

## 0.7

- feat: add `rest_of_path_urlencoded` and rename `rest` to `rest_of_path`
- feat: `http_of_dir`: redirect to index.html if present
- fix: `http_of_dir`: do not url-encode '/' in paths
- feat: add `Route.rest` to match the rest of the path
- feat: printing routes

## 0.6

- feat: add `Route.t` construct, deprecate scanf, add more structured path
- feat: use chunked encoding for large string responses, in addition to streams
- refactor(echo): simplify code, use gzip aggressively
- accept http1.0

- fix: do not output a `content-length` for a chunked response
- fix: set `transfer-encoding` header when returning a chunked stream
- fix(zip): handle case where camlzip consumes 0 bytes
- feat(zip): also compress string responses if they're big
- add more debug msg

## 0.5

- new `tiny_httpd_camlzip` library for handling `deflate` compression
- feat: expose `Headers.empty`
- fix: use the non-query path for routing
- feat(util): add some query related utils

## 0.4

- easy accessor to the query parameters in path
- fix: header field names are case insensitive
- doc: add note on jemalloc in the readme
- log error when closing client socket

## 0.3

- feat(http_of_dir): use `file` to guess mime type of file
- feat: allow handlers to take streams
- feat(bin): disable uploading by default
- feat: add `Tiny_httpd_util.parse_query` for query decoding
- feat(bin): set charset to utf8
- feat: autodetect ipv6 address
- feat: support ipv6 address

- fix: missing crlf between chunks
- fix: read_all must return rather than blocking when done
- fix: proper amortized O(1) push in Buf.push
- fix: `%X` for percent_encode; use `percent_decode` in `parse_query`

## 0.2

- feat(bin): count number of hidden files
- feat(bin): use `details` for hiding hidden files by default
- fix: improved percent encoding of paths
- feat: add percent encoding/decoding
- feat(bin): better human-size display
- feat: in http_of_dir, sort entries and display their size
- fix(http_of_dir): handle bad symlinks
- improve docs and opam, tidy up for 0.1
