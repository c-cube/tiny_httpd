#!/bin/sh
exec dune exec --display=quiet --profile=release "examples/echo_eio.exe" -- $@
