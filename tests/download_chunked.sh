#!/usr/bin/env sh

SERVER=$1
PORT=8084
"$SERVER" . -p $PORT &
PID=$!

sleep 0.1

echo download1 1>&2
curl -N "http://localhost:${PORT}/foo_50" -o data2 \
  -H 'Tranfer-encoding: chunked'

echo download2 1>&2
curl -N "http://localhost:${PORT}/foo_50" -o data2 \
  -H 'Tranfer-encoding: chunked'

echo download3 1>&2
curl -N "http://localhost:${PORT}/foo_50" -o data2 \
  -H 'Tranfer-encoding: chunked'

kill $PID
wc data2
