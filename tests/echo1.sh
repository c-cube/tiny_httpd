#!/usr/bin/env sh

ECHO=$1
PORT=8085

"$ECHO" -p $PORT &
PID=$!
sleep 0.1
curl -N "http://localhost:${PORT}/echo/?a=b&c=d" -H user-agent:test

sleep 0.1
curl -N "http://localhost:${PORT}/vfs/"

sleep 0.1
curl -N "http://localhost:${PORT}/vfs/a.txt"

sleep 0.1
curl -N "http://localhost:${PORT}/vfs/sub/yolo.html"

kill $PID
