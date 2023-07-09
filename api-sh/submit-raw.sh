#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 -x [JSON_FILENAME]

   default input file is stdin.
   submission schema JSON is required.
EOF
}


if [ x"${API_TOKEN}" = x ]; then
    API_TOKEN=$(cat token.txt)
fi


if [ x"$1" != x-x ]; then
    usage
    exit 1
fi
shift

contents_fn="$1"
if [ x"$contents_fn" = x ]; then
    input_cmd=cat
else
    input_cmd="cat $contents_fn"
fi

$input_cmd | curl -s -H "Authorization: Bearer ${API_TOKEN}" \
     -X POST \
     --json @- \
     https://api.icfpcontest.com/submission
