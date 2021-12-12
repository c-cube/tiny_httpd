#!/usr/bin/env sh

rm data

SERVER=$1
PORT=8087

"$SERVER" . -p $PORT --upload --max-upload 100000000000 &
PID=$!

sleep 0.1

cat foo_50 | curl -N -X PUT http://localhost:$PORT/data --data-binary @- -H 'Transfer-Encoding: chunked'

kill $PID
wc data
