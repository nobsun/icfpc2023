#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID
EOF
}


if [ x"$token" = x ]; then
    token=$(cat token.txt)
fi

problem_id="$1"

if [ x"$problem_id" = x ]; then
    usage
    exit 1
fi

curl -H "Authorization: Bearer ${token}" \
     https://api.icfpcontest.com/problem'?'problem_id=${problem_id} |
    jq -r '.Success'
