md: integrator.o potential.o md.f90
	gfortran md.f90 integrator.o potential.o -o md /usr/local/lib/libblas.a 

integrator.o: integrator.f90 potential.o
	gfortran -c integrator.f90

potential.o: potential.f90
	gfortran -c potential.f90