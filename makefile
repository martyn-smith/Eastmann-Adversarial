#all: TE results

TE:
	#gfortran -c teconstants.f95;
	gfortran -g3 -o debug.out -fall-intrinsics -fbacktrace -fdefault-real-8 \
	    -ffpe-trap=invalid,zero,overflow,underflow,denormal -fimplicit-none  \
		-Wall -static -std=f2003 temain.f95;
	gfortran -fall-intrinsics -fdefault-real-8 -O3 -std=f2003 temain.f95;
	./a.out;

results:
	@echo "making output folder"
	if [! -d '../../datasets/$date +"%d%m%C"' ]; then
	mkdir ../../datasets/$(date +"%d%m%C") # Control will enter here if $DIRECTORY exists.
	fi
	mv *.dat ../../datasets/$(date +"%d%m%C")

multiresults:
	for i in {1..64}
	do
	./a.out;
	./to_csv.sh;
	rm fout.dat;
	mv out.dat out$i.dat;
	mv inpt.dat inpt$i.dat;
	done
