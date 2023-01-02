diffusion: diffusion.f90 read.o
	gfortran diffusion.f90 read.o -o diffusion /usr/local/lib/libblas.a 

read.o: read.f90
	gfortran -c read.f90