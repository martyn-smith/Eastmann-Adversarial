#!/usr/bin/env bash
#study of affect of different aggression levels

echo "testing aggression"
results='aggression_ttf.txt'
aggression_levels='0.00001 0.0001 0.001 0.01 0.1'
touch $results
for i in $aggression_levels; do
    echo "testing" $i
    echo $i >> $results
    for j in {1..100} ; do
    .   ./builds/te_$(date +"%d%m%y") -a $i >> $results
    done
done