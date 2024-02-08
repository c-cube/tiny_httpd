# Tiny_httpd [![build](https://github.com/c-cube/tiny_httpd/workflows/build/badge.svg)](https://github.com/c-cube/tiny_httpd/actions)

Minimal HTTP server using good old threads, with stream abstractions,
simple routing, URL encoding/decoding, static asset serving,
and optional compression with camlzip.
It also supports [server-sent events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events)
([w3c](https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation))

Free from all forms of `ppx`, async monads, etc. 🙃

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

## Static assets and files

The program `http_of_dir` relies on the module `Tiny_httpd_dir`, which
can serve directories, as well as _virtual file systems_.

In 'examples/dune', we produce an OCaml module `vfs.ml` using
the program `tiny-httpd-vfs-pack`.  This module contains a VFS (virtual file
system) which can be served as if it were an actual directory.

The dune rule:

```lisp
(rule
  (targets vfs.ml)
  (deps (source_tree files) (:out test_output.txt.expected))
  (enabled_if (= %{system} "linux"))
  (action (run ../src/bin/vfs_pack.exe -o %{targets}
               --mirror=files/
               --file=test_out.txt,%{out}
               --url=example_dot_com,http://example.com)))
```

The code to serve the VFS from `vfs.ml` is as follows:

```ocaml
  …
  Tiny_httpd_dir.add_vfs server
    ~config:(Tiny_httpd_dir.config ~download:true
               ~dir_behavior:Tiny_httpd_dir.Index_or_lists ())
    ~vfs:Vfs.vfs ~prefix:"vfs";
  …
```

it allows downloading the files, and listing directories.
If a directory contains `index.html` then this will be served
instead of listing the content.

## Steaming response body

Tiny_httpd provides multiple ways of returning a body in a response.
The response body type is:

```ocaml
type body =
  [ `String of string
  | `Stream of byte_stream
  | `Writer of Tiny_httpd_io.Writer.t
  | `Void ]
```

The simplest way is to return, say, `` `String "hello" ``. The response
will have a set content-length header and its body is just the string.
Some responses don't have a body at all, which is where `` `Void `` is useful.

The `` `Stream _ `` case is more advanced and really only intended for experts.

The `` `Writer w `` is new, and is intended as an easy way to write the
body in a streaming fashion. See 'examples/writer.ml' to see a full example.
Typically the idea is to create the body with `Tiny_httpd_io.Writer.make ~write ()`
where `write` will be called with an output channel (the connection to the client),
and can write whatever it wants to this channel. Once the `write` function returns
the body has been fully sent and the next request can be processed.

## Socket activation

Since version 0.10, socket activation is supported indirectly, by allowing a
socket to be explicitly passed in to the `create` function:

```ocaml
module S = Tiny_httpd

let not_found _ _ = S.Response.fail ~code:404 "Not Found\n"

let () =
  (* Module [Daemon] is from the [ocaml-systemd] package *)
  let server = match Daemon.listen_fds () with
    (* If no socket passed in, assume server was started explicitly i.e. without
       socket activation *)
    | [] -> S.create ()

    (* If a socket passed in e.g. by systemd, listen on that *)
    | sock :: _ -> S.create ~sock ()
  in
  S.add_route_handler server S.Route.rest_of_path not_found;
  Printf.printf "Listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
```

On Linux, this requires the
[ocaml-systemd](https://github.com/juergenhoetzel/ocaml-systemd) package:

```
opam install ocaml-systemd
```

Tip: in the `dune` file, the package name should be `systemd`.

In case you're not familiar with socket activation, Lennart Poettering's
[blog post](http://0pointer.de/blog/projects/socket-activation.html) explains it
well.

## Why?

Why not? If you just want a super basic local server (perhaps for exposing
data from a local demon, like Cups or Syncthing do), no need for a ton of
dependencies or high scalability libraries.

Use cases might include:

- serve content directly from a static blog generator;
- provide a web UI to some tool (like CUPS and syncthing do);
- implement a basic monitoring page for a service;
- provide a simple json API for a service, on top of http;
- use `http_of_dir` to serve odoc-generated docs or some assets directory.

## Documentation

See https://c-cube.github.io/tiny_httpd

## License

MIT.
