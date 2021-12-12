#!/usr/bin/env sh

SSE_SERVER=$1
PORT=8086

"$SSE_SERVER" -p $PORT &
sleep 0.1

curl -N "http://localhost:${PORT}/count/10" -H user-agent:test
kill %1
