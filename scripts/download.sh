#!/bin/bash

PROBLEMS_DIR=problems

for i in `seq 1 45`
do
    curl -H "Accept: application/json" \
	 -H "Content-type: application/json" \
	 -X GET https://api.icfpcontest.com/problem?problem_id=${i} \
	| jq -r '.Success' | jq -c . > ${PROBLEMS_DIR}/$(printf "%03d.json" "${i}")
done
