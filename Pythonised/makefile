SHELL:= /bin/bash

report:
	rm -f *.png *.md *.h5 errors*.txt
	#if [command ../te]; then
	#    (cd ../ && make)
	#fi
	#if [!command pandoc]; then
	#    exit 1
	#fi
	#cp ../../models/stategenerator/ ./blue/stategenerator
	python teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm *.png *.md
