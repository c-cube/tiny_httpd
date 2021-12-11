#!/usr/bin/env sh

SERVER=$1
"$SERVER" . -p 8083 &

sleep 0.1

curl -N 'http://localhost:8083/foo_50' -o data2 \
  -H 'Tranfer-encoding: chunked'

kill %1
wc data2
