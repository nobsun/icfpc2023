#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID
EOF
}

problem_id="$1"

if [ x"$problem_id" = x ]; then
    usage
    exit 1
fi

json=problems/$(printf "%03d.json" "${problem_id}")
if [ -s "$json" ]; then
    cat <<EOF
$json already exists.
EOF
    exit 0
else
    mkdir -p problems
    ./api-sh/get-problem.sh ${problem_id} > ${json}
fi
