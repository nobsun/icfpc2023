#! /bin/sh


if [ x"$token" = x ]; then
    token=$(cat token.txt)
fi

set -x
curl -H "Authorization: Bearer ${token}" "$@"
