diffusion: diffusion.f90 read.o equations1.o
	gfortran diffusion.f90 read.o equations1.o -fbounds-check -o diffusion /usr/local/lib/libblas.a 

read.o: read.f90
	gfortran -c -fbounds-check read.f90

equations1.o: equations1.f90
	gfortran -c -fbounds-check equations1.f90
	
.PHONY: clean
clean:
	-rm -f *.o *.mod diffusion