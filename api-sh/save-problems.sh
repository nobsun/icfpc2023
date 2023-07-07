#! /bin/sh

problems=$(curl -H "Authorization: Bearer ${token}" \
                https://api.icfpcontest.com/problems |
               jq -r '.number_of_problems'
        )

if [ x"$problems" = x ]; then
    problems="$1"
fi

if [ x"$problems" = x ]; then
    cat <<EOF
Usage: $0 [NUM_OF_PROBLEMS]
EOF
    exit 1
fi

for i in $(seq 1 $problems)
do
    ./api-sh/save-problem.sh $i
done
