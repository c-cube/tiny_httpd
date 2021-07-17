#!/usr/bin/env bash

PORT=8082

dune exec ./sse_server.exe -- -p $PORT &
sleep 0.1
dune exec ./sse_client.exe -- -p $PORT --alarm=1 /count || true

kill %1
echo "success"
