#!/usr/bin/env sh

SSE_SERVER=$1

"$SSE_SERVER" -p 8083 &
sleep 0.1

curl -N 'http://localhost:8083/count/10' -H user-agent:test
kill %1
