SHELL:= /bin/bash
sdate := $(shell date +"%d%m%y")
ldate := $(shell date +"%Y-%m-%d")
cdate := $(shell date +"%d%m%C")

.PHONY: clean figures gym peaceful reports twin telib

tedbg: src/*.f95
	gfortran -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none \
		-g3 -Wall -Wno-unused-dummy-argument \
		-std=f2003 -o tedbg src/main.f95;

telib: src/*.f95
	gfortran -fall-intrinsics -fdefault-real-8 -fPIC \
		-O3 -shared -std=f2003 -o telib.so src/main.f95;

te: src/*.f95
	gfortran -fall-intrinsics -fdefault-real-8 \
		-O3 -std=f2003 -o te src/main.f95;

gymenv:
	poetry install

fuzzresults:
	for i in {1..64}
	do
	./te;
	./to_csv.sh;
	rm fout.dat;
	mv out.dat out$i.dat;
	mv inpt.dat inpt$i.dat;
	done

#peaceful split into its own, not least to reassure that the artist() errors are ignorable.
peaceful: clean gymenv te
	poetry run env/main.py -n 10 --blue none --red none --figures 10 \
		-o data/peaceful 2>> errors_$(ldate).txt
	#pandoc -V geometry:margin=0.5in -o report_peaceful_long_$(ldate).pdf report.md
	#rm -f *.png report.md

# twin run separately due to its time demands
twin: clean gymenv te
	poetry run env/main.py -n 100 -t 1h --blue twin --red none --figures 10 \
		-o data/blue_twin_red_none_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 100 -t 1h --blue twin --red continuous --figures 10 \
		-o data/blue_twin_red_continuous_$(ldate) 2>> errors_$(ldate).txt

test: clean gymenv te
	poetry run env/main.py -n 20 -t 1h --blue continuous --red continuous --report 10 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.5in -o report_test_$(ldate).pdf report.md
	rm -f *.png report.md

reports: clean gymenv te
	#validation
	poetry run env/main.py -n 100 -t 1h --blue discrete --red none --figures 10 \
		-o data/blue_discrete_red_none_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 100 -t 1h --blue continuous --red none --figures 10 \
		-o data/blue_continuous_red_none_$(ldate) 2>> errors_$(ldate).txt
	#baseline
	poetry run env/main.py -n 300 -t 1h --blue none --red discrete --figures 10 \
		-o data/blue_none_red_discrete_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 300 -t 1h --blue none --red discrete --intent destruction --figures 10 \
		-o data/blue_none_red_discrete_destruction_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 300 -t 1h --blue none --red discrete --intent recipe --figures 10 \
		-o data/blue_none_red_discrete_recipe_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 300 -t 1h --blue none --red continuous --figures 10 \
		-o data/blue_none_red_continuous_$(ldate) 2>> errors_$(ldate).txt
	#discrete
	poetry run env/main.py -n 300 -t 1h --blue discrete --red discrete --figures 10 \
		-o  data/blue_discrete_red_discrete_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 300 -t 1h --blue discrete --red discrete --intent destruction --figures 10 \
		-o data/blue_discrete_red_discrete_destruction_$(ldate) 2>> errors_$(ldate).txt
	poetry run env/main.py -n 300 -t 1h --blue discrete --red discrete --intent recipe --figures 10\
		-o data/blue_discrete_red_discrete_recipe_$(ldate) 2>> errors_$(ldate).txt
	#continuous
	poetry run env/main.py -n 300 -t 1h --blue continuous --red continuous --figures 10 \
		-o data/blue_continuous_red_continuous_$(ldate) 2>> errors_$(ldate).txt

clean:
	rm -f *.dat *.h5 *.mod *.png *.so errors*.txt report*.md te tedbg telib.so __pycache__
