#all: TE results

TE:
	gfortran temain.f95; ./a.out; ./to_csv.sh

results:
	@echo "making output folder"
	if [! -d '../../datasets/$date +"%d%m%C"' ]; then
	mkdir ../../datasets/$(date +"%d%m%C"  	# Control will enter here if $DIRECTORY exists.
	fi
	for file in 'out.dat' 'fout.dat' 'inpt.dat'
	do
	echo "reformatting"
	#replace leading spaces
	sed -i 's/^ */"/g' $file
	#replace middle spaces
	sed -i 's/  /","/g' $file
	#replace trailing
	sed -i 's/$/"/g' $file
	done
	mv *.dat ../../datasets/$(date +"%d%m%C")
