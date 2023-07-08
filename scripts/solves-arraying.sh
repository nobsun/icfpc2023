#! /bin/sh

problems=$(curl https://api.icfpcontest.com/problems | jq -r '.number_of_problems')

seq 1 $problems | xargs -P 4 -n 1 bin/solve arraying
