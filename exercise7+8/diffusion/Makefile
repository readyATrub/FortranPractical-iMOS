diffusion: diffusion.f90 read.o equations1.o
	gfortran diffusion.f90 read.o equations1.o -o diffusion /usr/local/lib/libblas.a 

read.o: read.f90
	gfortran -c read.f90

equations1.o: equations1.f90
	gfortran -c equations1.f90 

.PHONY: clean
clean:
	-rm -f *.o *.mod diffusion