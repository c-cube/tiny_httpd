#!/bin/bash

# usage: source stress.sh path/http_of_dir.exe nb
# it will run np parallel curl to fetch file $url below

SERVER=$1
nb=$2

PORT=8088
"$SERVER" . -p $PORT -j $nb &
PID=$!

url=http://localhost:${PORT}/foo_50
sleep_time=0

for (( c=1; c<=$nb; c++ )); do
  f=$(mktemp)
  (curl -s $url > $f; stat -c %s $f; diff foo_50 $f; rm $f) &
  PIDS[$c]=$!

  sleep $sleep_time
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

kill $PID
