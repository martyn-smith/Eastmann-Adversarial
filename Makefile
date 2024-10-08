SHELL:= /bin/bash
sdate := $(shell date +"%d%m%y")
ldate := $(shell date +"%Y-%m-%d")
cdate := $(shell date +"%d%m%C")

.PHONY: clean figures gym peaceful reports twin telib

te:
	cd Eastmann-95 && make te

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

# sensitivity
test: clean gymenv te
	poetry run env/main.py -n 100 -t 1h --blue continuous --red none --figures 100 \
		-o data/blue_continuous_high_eps_$(ldate) 2>> errors_$(ldate).txt

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
