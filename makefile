#all: TE results
SHELL:= /bin/bash
date:= $(shell date +"%Y-%m-%d")
sdate:= $(shell date +"%d%m%y")

TE:
	gfortran -g3 -o tedbg_$(sdate) -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -std=f2003 -fbackslash src/main.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -fbackslash -o te_$(sdate) src/main.f95;
	ln -s -f te_$(sdate) te;
	./te_$(sdate);
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
	if ! [ -d "Pythonised/stategenerator" ]; then \
		cp -r ../models/saved/stategenerator/ Pythonised/; \
	else \
		echo "found model: skipping..."; \
	fi \

report: clean TE model
	Pythonised/main.py --fast --report 2>> errors_$(date).txt
	pandoc -o report_$(date).pdf report_$(date).md
	rm -f *.png report*.md

figures: clean TE model
	Pythonised/main.py --fast --report 2>> errors_$(date).txt

controls: clean TE model
	Pythonised/main.py --fast --report 2>> errors_$(date).txt
	pandoc -o report_$(date).pdf report_$(date).md
	rm -f *.png report*.md
	Pythonised/main.py --fast --nored --report 2>> errors_$(date).txt
	pandoc -o report_nored_$(date).pdf report_$(date).md
	rm -f *.png report*.md
	Pythonised/main.py --fast --noblue --report 2>> errors_$(date).txt
	pandoc -o report_noblue_$(date).pdf report_$(date).md
	rm -f *.png report*.md

scenarios: clean TE model
	Pythonised/main.py --fast --intent downtime --report 2>> errors_$(date).txt
	pandoc -o report_downtime_$(date).pdf report_$(date).md
	rm -f *.png report*.md
	Pythonised/main.py --fast --intent recipe --report 2>> errors_$(date).txt
	pandoc -o report_recipe_$(date).pdf report_$(date).md
	rm -f *.png report*.md
	Pythonised/main.py --fast --intent destruction --report 2>> errors_$(date).txt
	pandoc -o report_destruction_$(date).pdf report_$(date).md
	rm -f *.png report*.md
	Pythonised/main.py --fast --nored --report 2>> errors_$(date).txt
	pandoc -o report_nored_$(date).pdf report_$(date).md
	rm -f *.png report*.md
	Pythonised/main.py --fast --noblue --report 2>> errors_$(date).txt
	pandoc -o report_noblue_$(date).pdf report_$(date).md
	rm -f *.png report*.md
