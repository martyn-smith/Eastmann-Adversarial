#!/usr/bin/env bash
#study of affect of different aggression levels

echo "testing idvs"
results='idv_ttf.txt'

touch $results
for i in {1..24}; do
    echo "testing" $i
    echo $i >> $results
    for j in {1..100} ; do
        ../builds/te_$(date +"%d%m%y") -d $i >> $results
    done
done