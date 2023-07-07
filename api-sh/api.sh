#! /bin/sh


if [ x"${API_TOKEN}" = x ]; then
    API_TOKEN=$(cat token.txt)
fi

set -x
curl -H "Authorization: Bearer ${API_TOKEN}" "$@"
