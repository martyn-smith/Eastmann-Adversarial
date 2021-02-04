#!/usr/bin/env bash

echo "testing setpoint min/max"
results='./setpoint_minmax_ttf.txt'
touch $results
echo setpoint >> $results
declare -a min_values=(-25.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
declare -a max_values=(999.0 199.0 199.0 199.0 999.0 99.0 9999.0 99.0 99.0)
for i in {1..9} ; do
    echo "/ntesting setpoint" $i ${min_values[$i-1]} ${max_values[$i-1]} >> $results
    ./builds/te_$(date +"%d%m%y") --mode $i SET ${min_values[$i-1]} >> $results
    ./builds/te_$(date +"%d%m%y") --mode $i SET ${max_values[$i-1]} >> $results
done

#sed -i 's/^ */"/g' $file