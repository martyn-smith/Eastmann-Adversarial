#all: TE results
SHELL:= /bin/bash

TE:
	gfortran -g3 -o tedbg_$$(date +"%d%m%y") -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -std=f2003 -fbackslash src/temain.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -fbackslash -o te_$$(date +"%d%m%y") src/temain.f95;
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

model:
	cp -r ../models/saved/stategenerator/ Pythonised/

report: clean TE model
	python Pythonised/teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md

figures: clean TE model
	python Pythonised/teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt

scenarios: clean TE model
	python Pythonised/teprob.py --fast --intent downtime --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_downtime_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	python Pythonised/teprob.py --fast --intent recipe --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_recipe_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	python Pythonised/teprob.py --fast --intent destruction --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_destruction_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	python Pythonised/teprob.py --fast --nored --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_nored_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	python Pythonised/teprob.py --fast --noblue --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_noblue_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
