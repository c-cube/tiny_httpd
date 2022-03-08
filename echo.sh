#!/bin/sh
exec dune exec --profile=release "examples/echo.exe" -- $@
