#!/usr/bin/env sh
echo echo1 1>&2

ECHO=$1
PORT=8085

"$ECHO" -p $PORT &
PID=$!
sleep 0.1
curl -N "http://localhost:${PORT}/echo/?a=b&c=d" -H user-agent:test
kill $PID
