#!/usr/bin/env sh

ECHO=$1
PORT=$2

"$ECHO" -p $PORT &
PID=$!
sleep 0.1

echo === echo $ECHO $PORT 1>&2
curl -N "http://localhost:${PORT}/echo/?a=b&c=d" -H user-agent:test --max-time 5

sleep 0.1
echo === echo $ECHO $PORT 1>&2
curl -N "http://localhost:${PORT}/vfs/" --max-time 5

sleep 0.1
echo === echo $ECHO $PORT 1>&2
curl -N "http://localhost:${PORT}/vfs/a.txt" --max-time 5

sleep 0.1
echo === echo $ECHO $PORT 1>&2
curl -N "http://localhost:${PORT}/vfs/sub/yolo.html" --max-time 5

kill $PID
