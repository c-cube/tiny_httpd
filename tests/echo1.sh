#!/usr/bin/env sh

ECHO=$1
PORT=8085

"$ECHO" -p $PORT &
sleep 0.1
curl -N "http://localhost:${PORT}/echo/?a=b&c=d" -H user-agent:test
kill %1
