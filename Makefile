SHELL:= /bin/bash
sdate := $(shell date +"%d%m%y")
ldate := $(shell date +"%Y-%m-%d")
cdate := $(shell date +"%d%m%C")

.PHONY: clean figures gym peaceful reports twin

te:
	gfortran -g3 -o tedbg -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -Wno-unused-dummy-argument -std=f2003 src/main.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 -o te src/main.f95;

gymenv:
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
peaceful: clean gymenv te
	poetry run env/main.py -n 10 --blue none --red none --report 10 2>> errors_$(ldate).txt
	pandoc -V geometry:margin=0.5in -o report_peaceful_long_$(ldate).pdf report.md
	rm -f *.png report.md

continuous:
	poetry run env/main.py -n 1000 -t 1h --blue continuous --red continuous --report 100 2>> errors_$(ldate).txt
	mkdir blue_continuous_red_continuous_$(ldate) && mv *.png blue_continuous_red_continuous_$(ldate)

reports: clean gymenv te
	#validation
	poetry run env/main.py -n 100 -t 1h --blue discrete --red none --report 10 2>> errors_$(ldate).txt
	mkdir discrete_validation_$(ldate) && mv *.png discrete_validation_$(ldate)/
	#pandoc -V geometry:margin=0.5in -o report_blue_discrete_validation_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py -n 100 -t 1h --blue continuous --red none --report 10 2>> errors_$(ldate).txt
	mkdir continuous_validation_$(ldate) && mv *.png continuous_validation_$(ldate)/
	#pandoc -V geometry:margin=0.5in -o report_blue_continuous_validation_$(ldate).pdf report.md
	rm -f *.png report.md
	#baseline
	poetry run env/main.py -n 300 -t 1h --blue none --red discrete --report 10 2>> errors_$(ldate).txt
	mkdir red_discrete_baseline_$(ldate) && mv *.png red_discrete_baseline_$(ldate)/
	#pandoc -V geometry:margin=0.5in -o report_red_discrete_baseline_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py -n 300 -t 1h --blue none --red continuous --report 10 2>> errors_$(ldate).txt
	mkdir red_continuous_baseline_$(ldate) && mv *.png red_continuous_baseline_$(ldate)/
	#pandoc  -V geometry:margin=0.5in -o report_red_continuous_baseline_$(ldate).pdf report.md
	rm -f *.png report.md
	#discrete
	poetry run env/main.py -n 300 -t 1h --blue discrete --red discrete --report 10 2>> errors_$(ldate).txt
	mkdir blue_discrete_red_discrete_$(ldate) && mv *.png blue_discrete_red_discrete_$(ldate)/
	#pandoc -V geometry:margin=0.5in -o report_blue_discrete_red_discrete_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py -n 300 -t 1h --blue discrete --red discrete --intent destruction \
		--report 10 2>> errors_$(ldate).txt
	mkdir blue_discrete_red_discrete_intent_destruction_$(ldate) \
		&& mv *.png blue_discrete_red_discrete_intent_destruction_$(ldate)/
	#pandoc -o report_blue_discrete_red_discrete_destruction_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py -n 300 -t 1h --blue discrete --red discrete --intent recipe \
		--report 10 2>> errors_$(ldate).txt
	mkdir blue_discrete_red_discrete_intent_recipe_$(ldate) \
		&& mv *.png blue_discrete_red_discrete_intent_recipe_$(ldate)/
	#pandoc  -V geometry:margin=0.5in -o report_blue_discrete_red_discrete_recipe_$(ldate).pdf report.md
	rm -f *.png report.md
	#continuous
	poetry run env/main.py -n 300 -t 1h --blue continuous --red continuous --report 10 2>> errors_$(ldate).txt
	mkdir blue_continuous_red_continuous_$(ldate) && mv *.png blue_continous_red_continous_$(ldate)
	#pandoc  -V geometry:margin=0.5in -o report_blue_continuous_red_continuous_$(ldate).pdf report.md
	rm -f *.png report.md

# twin run separately due to its time demands
twin: clean gymenv te
	poetry run env/main.py -n 100 -t 1h --blue twin --red none --report 10 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.5in -o report_blue_twin_validation_$(ldate).pdf report.md
	rm -f *.png report.md
	poetry run env/main.py -n 100 -t 1h --blue twin --red continuous --report 10 2>> errors_$(ldate).txt
	pandoc  -V geometry:margin=0.5in -o report_blue_twin_red_continuous_$(ldate).pdf report.md
	rm -f *.png report.md

test: clean gymenv te
	poetry run env/main.py -n 20 -t 1h --blue continuous --red continuous --report 10 2>> errors_$(ldate).txt
	mkdir discrete_validation_$(ldate) && mv *.png discrete_validation_$(ldate)/
	#pandoc  -V geometry:margin=0.5in -o report_test_$(ldate).pdf report.md
