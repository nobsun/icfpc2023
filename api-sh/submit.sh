#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID [CONTENTS_FILE]

   default contents file is stdin
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

contents_fn="$2"
if [ x"$contents_fn" = x ]; then
    json_str=$(cat | runghc api-sh/json-escape.hs)
else
    json_str=$(cat $contents_fn | runghc api-sh/json-escape.hs)
fi

show_json() {
    cat <<EOF
{ "problem_id": $problem_id, "contents": $json_str }
EOF
}

show_json | curl -H "Authorization: Bearer ${token}" \
     -X POST \
     --json @- \
     https://api.icfpcontest.com/submission
