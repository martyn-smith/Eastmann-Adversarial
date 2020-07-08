for file in 'out.dat' 'fout.dat' 'inpt.dat' 'dvec.dat'
do
#replace leading spaces
sed -i 's/^ */"/g' $file
#replace middle spaces
sed -i 's/  /","/g' $file
#replace trailing
sed -i 's/$/"/g' $file
done
