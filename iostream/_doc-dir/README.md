# Iostream

[![Build and Test](https://github.com/c-cube/ocaml-iostream/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/ocaml-iostream/actions/workflows/main.yml)

This library defines _generic_ I/O streams of bytes. The streams should be
composable, user-definable, and agnostic to the underlying I/O mechanism; with
OCaml 5 it means that they might be backed by an effect-based scheduler.

The goal is to provide a reasonable interoperability layer that multiple libraries and applications
in the OCaml ecosystem can rely on, while providing the modularity that standard IO channels lack.
Modern statically typed languages like Go and Rust provide this layer in their stdlib and their whole
ecosystem can build on it.

## Documentation

https://c-cube.github.io/ocaml-iostream/

## License

MIT license.
