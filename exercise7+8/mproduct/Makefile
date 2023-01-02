mproduct: matrix.o system.o mproduct.f90
	gfortran mproduct.f90 matrix.o system.o -o mproduct /usr/local/lib/libblas.a 

matrix.o: matrix.f90
	gfortran -c matrix.f90

system.o: system.f90
	gfortran -c system.f90
