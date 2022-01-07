report:
	touch foo.png bar.md
	rm *.png *.md
	python3 teprob.py --fast --report 2>> errors_$$(date +"%Y-%m-%d").txt
	pandoc -o report_$$(date +"%Y-%m-%d").pdf report_$$(date +"%Y-%m-%d").md
	rm *.png *.md
