#! /bin/sh

ls -1 solutions/*_???.json | xargs -P 15 -n 1 ./scripts/try-submit.sh
