#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID [LIMIT]
EOF
}


if [ x"${API_TOKEN}" = x ]; then
    API_TOKEN=$(cat token.txt)
fi

problem_id="$1"

if [ x"$problem_id" = x ]; then
    usage
    exit 1
fi

limit=10

if [ x"$2" != x ]; then
    limit=$2
fi

CURL_SILENT=-s

curl $CURL_SILENT -H "Authorization: Bearer ${API_TOKEN}" \
     https://api.icfpcontest.com/submissions"?offset=0&limit=${limit}&problem_id=${problem_id}"
