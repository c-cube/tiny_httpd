#!/bin/sh
exec dune exec --display=quiet src/bin/vfs_pack.exe --profile=release -- $@
