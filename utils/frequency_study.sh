#!/usr/bin/env bash

echo "testing sine wave disturbances"
results='./periodic_sine_ttf_2.txt'
amplitudes='50.0 20.0 10.0 5.0 2.0 1.0 0.5 0.2 0.1 0.01'
periods='1.0 2.0 5.0 10.0 20.0 50.0 100.0 200.0'
touch $results
echo xmv amplitude period error >> $results
for i in {1..12} ; do
    echo "testing xmv" $i
    for amp in $amplitudes; do
        for period in $periods; do
            echo -n $'\n' $i ' ' $amp ' ' $period >> $results
            ./builds/te_$(date +"%d%m%y") --xmv $i SINE $period $amp >> $results
        done
    done
done
echo "testing square wave disturbances"
results='./periodic_square_ttf_2.txt'
touch $results
echo xmv amplitude period error >> $results
for i in {1..12} ; do
    echo "testing xmv" $i
    for amp in $amplitudes; do
        for period in $periods; do
            echo -n $'\n' $i ' ' $amp ' ' $period >> $results
            ./builds/te_$(date +"%d%m%y") --xmv $i SQUARE $period $amp >> $results
        done
    done
done
#sed -i 's/^ */"/g' $file