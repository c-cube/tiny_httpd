#!/usr/bin/env bash

PORT=8082

./sse_server.exe -p $PORT &
PID=$!

sleep 0.1
./sse_client.exe -p $PORT --alarm=1 /count | tr -d '\r' || true

kill $PID
echo "success"

if [ -f ./sse_server_domains.exe ]; then
  ./sse_server_domains.exe -p $PORT &
  PID=$!

  sleep 0.1
  ./sse_client.exe -p $PORT --alarm=1 /count | tr -d '\r' || true

  kill $PID
  echo "success with domains"
fi
