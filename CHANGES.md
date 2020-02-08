
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
