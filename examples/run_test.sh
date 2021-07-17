#!/usr/bin/env bash

PORT=8082

./sse_server.exe -p $PORT &
sleep 0.1
./sse_client.exe -p $PORT --alarm=1 /count | tr -d '\r' || true

kill %1
echo "success"
