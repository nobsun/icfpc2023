#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 SUBMISSION_ID
EOF
}


if [ x"${API_TOKEN}" = x ]; then
    API_TOKEN=$(cat token.txt)
fi

submission_id="$1"

if [ x"$submission_id" = x ]; then
    usage
    exit 1
fi

CURL_SILENT=-s

curl $CURL_SILENT -H "Authorization: Bearer ${API_TOKEN}" \
     https://api.icfpcontest.com/submission"?&submission_id=${submission_id}"
