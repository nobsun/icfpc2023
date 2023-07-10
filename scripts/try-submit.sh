#! /bin/sh

path="$1"

log=slogs/$(basename ${path} .json).log

./bin/try-submit "${path}" | tee ${log}
