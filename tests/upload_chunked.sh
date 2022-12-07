#!/usr/bin/env sh

if [ -f data ]; then rm data ; fi

SERVER=$1
PORT=$2

"$SERVER" . -p $PORT --upload --max-upload 100000000000 &
PID=$!

sleep 0.1

echo === upload $SERVER $PORT 1>&2
cat foo_50 | curl -N -X PUT http://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10

kill $PID
wc data
