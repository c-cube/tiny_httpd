#!/usr/bin/env sh

SERVER=$1
PORT=8084
"$SERVER" . -p $PORT &
PID=$!

sleep 0.1

curl -N "http://localhost:${PORT}/foo_50" -o data2 \
  -H 'Tranfer-encoding: chunked'

kill $PID
wc data2
