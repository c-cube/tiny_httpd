#!/usr/bin/env sh

ECHO=$1
PORT=8085

"$ECHO" -p $PORT &
PID=$!
sleep 0.1
curl -N "http://localhost:${PORT}/echo/?a=b&c=d" -H user-agent:test --max-time 5

sleep 0.1
curl -N "http://localhost:${PORT}/vfs/" --max-time 5

sleep 0.1
curl -N "http://localhost:${PORT}/vfs/a.txt" --max-time 5

sleep 0.1
# NOTE: the sed is there because of a timing/deflate non determinism. Both strings
# decompress to the same "hello\nworld\n" but which one is picked depends on
# the machine/library/… ?? but both are valid.
curl -N "http://localhost:${PORT}/vfs/a.txt" -H 'accept-encoding: deflate' --max-time 5 | base64 | sed 's+ykjNycnnKs8vyknhAgAAAP//AwA=+ykjNycnnKs8vyknhAgAAAP//+'

sleep 0.1
curl -N "http://localhost:${PORT}/vfs/sub/yolo.html" --max-time 5

kill $PID
