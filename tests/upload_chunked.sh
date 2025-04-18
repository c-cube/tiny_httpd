#!/usr/bin/env sh

if [ -f data ]; then rm data ; fi

SERVER=$1
PORT=8087

"$SERVER" . -p $PORT --upload --max-upload 1000M &
PID=$!

sleep 0.1

cat foo_50 | curl -N -X PUT http://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10

kill $PID
echo ''
wc -m data
