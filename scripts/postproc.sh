#! /bin/sh

pnum="$1"
json="$2"

base=$(basename $json)

set -x
./bin/postprocess tune-volume $pnum $json -o postapply/tunev-$base | tee plog/tunev-${base}.log
./bin/postprocess swap $pnum $json -o postapply/step-$base --steps 200 | tee plog/swap-${base}.log
