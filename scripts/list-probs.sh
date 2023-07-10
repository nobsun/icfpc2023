#! /bin/sh

for i in $(seq 1 90) ; do
    nnn=$(printf "%03d" $i)
    for fn in solutions/*_${nnn}.json ; do
        echo ./scripts/postproc.sh $i $fn
    done
done
