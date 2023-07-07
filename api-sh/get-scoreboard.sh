#! /bin/sh

if [ x"$token" = x ]; then
    API_TOKEN=$(cat token.txt)
fi

curl -H "Authorization: Bearer ${API_TOKEN}" \
     https://api.icfpcontest.com/scoreboard
