
# Tiny_httpd [![build](https://github.com/c-cube/tiny_httpd/workflows/build/badge.svg)](https://github.com/c-cube/tiny_httpd/actions)

Minimal HTTP server using good old threads, with stream abstractions,
simple routing, URL encoding/decoding, and optional compression with camlzip.

Free from all forms of `ppx`, async monads, etc.

**Note**: it can be useful to add the `jemalloc` opam package for long running
server, as it does a good job at controlling memory usage.

The basic echo server from `src/examples/echo.ml`:

```ocaml

module S = Tiny_httpd

let () =
  let server = S.create () in
  (* say hello *)
  S.add_route_handler ~meth:`GET server
    S.Route.(exact "hello" @/ string @/ return)
    (fun name _req -> S.Response.make_string (Ok ("hello " ^name ^"!\n")));
  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req -> S.Response.make_string (Ok (Format.asprintf "echo:@ %a@." S.Request.pp req)));
  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
```

```sh
$ dune exec src/examples/echo.exe &
listening on http://127.0.0.1:8080

# the path "hello/name" greets you.
$ curl -X GET http://localhost:8080/hello/quadrarotaphile
hello quadrarotaphile!

# the path "echo" just prints the request.
$ curl -X GET http://localhost:8080/echo --data "howdy y'all" 
echo:
{meth=GET;
 headers=Host: localhost:8080
         User-Agent: curl/7.66.0
         Accept: */*
         Content-Length: 10
         Content-Type: application/x-www-form-urlencoded;
 path="/echo"; body="howdy y'all"}

```

## `http_of_dir`

Similar to `python -m http.server`, a simple program `http_of_dir` is provided.
It serves files from the current directory.

```sh
$ http_of_dir . -p 8080 &
$ curl -X GET http://localhost:8080
...
<html list of current dir>
...

```


## Why?

Why not? If you just want a super basic local server (perhaps for exposing
data from a local demon, like Cups or Syncthing do), no need for a ton of
dependencies or high scalability libraries.

Use cases might include:

- serve content directly from a static blog generator
- provide a web UI to some tool (like CUPS and syncthing do)
- implement a basic monitoring page for a service
- provide a simple json API for a service, on top of http
- use `http_of_dir` to serve odoc-generated docs or some assets directory

## Documentation

See https://c-cube.github.io/tiny_httpd

## License

MIT.


