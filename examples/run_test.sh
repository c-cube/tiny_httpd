#!/usr/bin/env bash

SERVER=$1
PORT=$2

./$SERVER -p $PORT &
PID=$!

sleep 0.1
./sse_client.exe -p $PORT --alarm=1 /count | tr -d '\r' || true

kill $PID
echo "success"
