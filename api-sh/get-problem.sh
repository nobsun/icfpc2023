#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID
EOF
}


problem_id="$1"

if [ x"$problem_id" = x ]; then
    usage
    exit 1
fi

curl https://api.icfpcontest.com/problem'?'problem_id=${problem_id} | \
    jq -r '.Success'
