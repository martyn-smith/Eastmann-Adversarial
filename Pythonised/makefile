SHELL:= /bin/bash

report:
	rm -f *.png *.md *.h5 errors*.txt
	python3 teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm *.png *.md
