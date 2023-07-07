#! /bin/sh

if [ x"$token" = x ]; then
    token=$(cat token.txt)
fi

curl -H "Authorization: Bearer ${token}" \
     https://api.icfpcontest.com/scoreboard
