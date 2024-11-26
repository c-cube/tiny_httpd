#!/usr/bin/env sh

ECHO=$1
PORT=8085

"$ECHO" -p $PORT &
PID=$!
sleep 0.1
echo "test moonpool_io"
curl -N "http://localhost:${PORT}/echo/?a=b&c=d" -H user-agent:test --max-time 5

kill $PID
