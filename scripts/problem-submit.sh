#! /bin/sh
path="$1"

log=plogs/problem-${1}.log

./bin/problem-submit "$@" | tee $log
