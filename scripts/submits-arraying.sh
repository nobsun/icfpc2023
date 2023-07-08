#! /bin/sh

for i in \
    1 2 3 4 5 6 7 8 9 11 12 15 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56
do
    path=$(printf "solutions/arraying_%03d.json" $i)
    submit=${path}.submit
    if [ -r ${submit} ]; then
        echo "alreay submitted, skip $path"
    else
        set -x
        ( ./api-sh/submit.sh $i $path ; /bin/echo ) > ${submit}
        set +x
    fi
done