#!/usr/bin/env sh

rm data

SERVER=$1
"$SERVER" . -p 8083 --upload --max-upload 100000000000 &

sleep 0.1

cat foo_50 | curl -N -X PUT http://localhost:8083/data --data-binary @- -H 'Transfer-Encoding: chunked'

kill %1
wc data
