#!/bin/sh
OPTS="--display=quiet --profile=release"
exec dune exec $OPTS ./src/bin/http_of_dir.exe -- $@
