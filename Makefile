#all: TE results
SHELL:= /bin/bash
sdate := $(shell date +"%d%m%y")
ldate := $(shell date +"%Y-%m-%d")
cdate := $(shell date +"%d%m%C")

build:
	gfortran -g3 -o tedbg_$(sdate) -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -std=f2003 src/main.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -o te_$(sdate) src/main.f95;
	ln -s -f te_$(sdate) te;
	./te_$(sdate);

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

reports: build install clean
	poetry run env/main.py --fast --report --blue none --red none -n 100 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_peaceful_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue discrete --red none -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_blue_discrete_validation_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py --fast --report --blue discrete --red continuous -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_blue_discrete_red_continuous_$(ldate).pdf report.md
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
	poetry run env/main.py --fast --report --blue continuous --red continuous -n 300 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.8in -o report_blue_discrete_red_discrete_recipe_$(ldate).pdf report.md
	rm -f *.png report.md

report: build install clean
	poetry run env/main.py --fast --report --blue continuous --red continuous -n 300 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.8in -o report_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md

figures: build clean
	poetry run env/main.py --fast --report -n 300 2>> errors_$(ldate).txt

nored: build clean
	poetry run env/main.py --fast --red none --report -n 300 2>> errors_$(ldate).txt
	pandoc -o report_nored_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md

noblue: build clean
	poetry run env/main.py --fast --blue none --report -n 300 2>> errors_$(ldate).txt
	pandoc -o report_noblue_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md

controls: build install clean
	poetry run env/main.py --fast --report -n 300 2>> errors_$(ldate).txt
	pandoc -o report_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
	poetry run env/main.py --fast --red none --report -n 300 2>> errors_$(ldate).txt
	pandoc -o report_nored_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
	poetry run env/main.py --fast --blue none --report -n 300 2>> errors_$(ldate).txt
	pandoc -o report_noblue_$(ldate).pdf report_$(ldate).md

scenarios: build clean
	poetry run env/main.py --fast --intent downtime --report 2>> errors_$(ldate).txt
	pandoc -o report_downtime_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
	poetry run env/main.py --fast --intent recipe --report 2>> errors_$(ldate).txt
	pandoc -o report_recipe_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
	poetry run env/main.py --fast --intent destruction --report 2>> errors_$(ldate).txt
	pandoc -o report_destruction_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
	poetry run env/main.py --fast --red none --report 2>> errors_$(ldate).txt
	pandoc -o report_nored_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
	poetry run env/main.py --fast --blue none --report 2>> errors_$(ldate).txt
	pandoc -o report_noblue_$(ldate).pdf report_$(ldate).md
	rm -f *.png report*.md
