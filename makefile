#all: TE results
SHELL:= /bin/bash

build:
	gfortran -g3 -o tedbg_$$(date +"%d%m%y") -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -std=f2003 src/main.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -o te_$$(date +"%d%m%y") src/main.f95;
	ln -s -f te_$$(date +"%d%m%y") te;
	./te_$$(date +"%d%m%y");

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
	rm -f *.mod *.png report*.md *.h5 *.dat errors*.txt  te_* tedbg_* __pycache__

report: build clean
	Pythonised/main.py --fast --report --data 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md

figures: build clean
	Pythonised/main.py --fast --report --data 2>> errors_$$(date +"%Y-%m-%d").txt

controls: build clean
	Pythonised/main.py --fast --report --data 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	Pythonised/main.py --fast --nored --report --data 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_nored_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	Pythonised/main.py --fast --noblue --report --data 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_noblue_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md

scenarios: build clean
	Pythonised/main.py --fast --intent downtime --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_downtime_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	Pythonised/main.py --fast --intent recipe --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_recipe_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	Pythonised/main.py --fast --intent destruction --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_destruction_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	Pythonised/main.py --fast --nored --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_nored_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
	Pythonised/main.py --fast --noblue --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_noblue_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm -f *.png report*.md
