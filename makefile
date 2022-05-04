#all: TE results
SHELL:= /bin/bash

TE:
	gfortran -g3 -o tedbg_$$(date +"%d%m%y") -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -std=f2003 src/temain.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -o te_$$(date +"%d%m%y") src/temain.f95;
	ln -s -f te_$$(date +"%d%m%y") te;
	./te_$$(date +"%d%m%y");
	rm *.mod;

moveresults:
	@echo "making output folder"
	if [! -d '../../datasets/$date +"%d%m%C"' ]; then
	mkdir ../../datasets/$(date +"%d%m%C") # Control will enter here if $DIRECTORY exists.
	fi
	mv *.dat ../../datasets/$(date +"%d%m%C")

fuzzresults:
	for i in {1..64}
	do
	./te;
	./to_csv.sh;
	rm fout.dat;
	mv out.dat out$i.dat;
	mv inpt.dat inpt$i.dat;
	done

clean:
	rm -f *.mod *.png report*.md *.h5 errors*.txt te_* tedbg_*

report: TE clean
	cp -r ../models/stategenerator/ Pythonised/
	python Pythonised/teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm *.png *.md

figures: TE clean
	cp -r ../models/stategenerator/ Pythonised/
	python Pythonised/teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt
