echo "testing minmax"
results='./minmax_ttf.txt'
touch $results
echo xmv amplitude period error >> $results
for xmv in {1..12} ; do
    echo -n $'\n' $xmv ' ' MAX >> $results
    for i in {1..100}; do
    ./builds/te_$(date +"%d%m%y") --xmv $xmv MAX >> $results
    done
    echo -n $'\n' $xmv ' ' MIN >> $results
    for i in {1..100}; do
    ./builds/te_$(date +"%d%m%y") --xmv $xmv MIN >> $results
    done
done
#sed -i 's/^ */"/g' $file