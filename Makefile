#all: TE results
SHELL:= /bin/bash
sdate := $(shell date +"%d%m%y")
ldate := $(shell date +"%Y-%m-%d")
cdate := $(shell date +"%d%m%C")

te:
	gfortran -g3 -o tedbg -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -Wno-unused-dummy-argument -std=f2003 src/main.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -o te src/main.f95;

install:
	poetry install

moveresults:
	@echo "making output folder"
	@if [! -d '../../datasets/$(cdate)' ]; then
	mkdir ../../datasets/$(cdate) # Control will enter here if $DIRECTORY exists.
	@fi
	@if [! -d '../../datasets/$(cdate)]; then
	mkdir ../../datasets/$(cdate) # Control will enter here if DIRECTORY exists.
	@fi
	mv *.dat ../../datasets/$(cdate)

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

#peaceful split into its own, not least to reassure that the artist() errors are ignorable.
peaceful: te install clean
	poetry run env/main.py --report --blue none --red none -n 11
	pandoc -V geometry:margin=0.8in -o report_peaceful_long_$(ldate).pdf report.md
	rm -f *.png report.md

reports: te install clean
	#discrete
	poetry run env/main.py --fast --report --blue discrete --red none -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_blue_discrete_validation_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue none --red discrete -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_red_discrete_baseline_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue discrete --red discrete -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_blue_discrete_red_discrete_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue discrete --red discrete --intent destruction -n 300 2>> errors_$(ldate).txt
	pandoc -o report_blue_discrete_red_discrete_destruction_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue discrete --red discrete --intent recipe -n 300 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.8in -o report_blue_discrete_red_discrete_recipe_$(ldate).pdf report.md
	rm -f *.png report.md
	#continuous
	poetry run env/main.py --fast --report --blue none --red continuous -n 300 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.8in -o report_red_continuous_baseline_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue continuous --red continuous -n 300 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.8in -o report_blue_continuous_red_continuous_$(ldate).pdf report.md
	rm -f *.png report.md
	#twin
	poetry run env/main.py --fast --report --blue twin --red none -n 300 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.8in -o report_blue_twin_validation_$(ldate).pdf report.md
	rm -f *.png report.md

twin: clean te install
	poetry run env/main.py --fast --report --blue twin --red continuous -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_$(ldate).pdf report.md
	rm -f *.png report*.md

figures: te clean
	poetry run env/main.py --fast --blue discrete --red discrete --report -n 11 2>> errors_$(ldate).txt
