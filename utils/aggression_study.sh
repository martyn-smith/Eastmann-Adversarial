for i in {5..1} ; do
    echo "testing" $i
    echo $i >> aggression_ttf.txt
    for j in {1..100} ; do
    ./bin/aggression_adjustable.out $((10 ** i)) >> aggression_ttf.txt
    done
done