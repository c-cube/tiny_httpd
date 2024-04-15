#!/usr/bin/env sh

ECHO=$1
PORT=8096

PINGS=0 "$ECHO" -p $PORT >/dev/null 2>/dev/null &
PID=$!
sleep 0.1

trap "echo 'exit' && kill $PID" EXIT

( cd ws-client && cargo build -p tinyhttpd-ws-client && cp target/debug/tinyhttpd-ws-client .. ) || exit 1

echo "pwd: $PWD"
(cd ./"$(dirname $0)/ws-client/" && 
  ( (cat testfile; sleep 1) \
    | ./tinyhttpd-ws-client tunnel "ws://localhost:${PORT}/echo" ))


