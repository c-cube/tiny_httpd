#!/usr/bin/env sh

ECHO=$1

"$ECHO" -p 8083 &
sleep 0.1
curl -N 'http://localhost:8083/echo/?a=b&c=d' -H user-agent:test
kill %1
