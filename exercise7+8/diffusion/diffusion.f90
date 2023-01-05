PROGRAM diffusion
    USE read
    USE equations
    IMPLICIT NONE

    CHARACTER(LEN=200) :: filename
    INTEGER :: nsteps, nframes, nmols, nsites,i,j,k,ios
    REAL :: dt,dcoeff,startf,endf
    REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: input
    
    PRINT '("Enter trajectory file (in quotation marks):")'
    READ(*,*) filename
    PRINT '("Enter number of water molecules:")'
    READ(*,*) nmols
    PRINT '("Enter number of sites:")'
    READ(*,*) nsites
    PRINT '("Enter number of time steps:")'
    READ(*,*) nsteps
    PRINT '("Enter time step size:")'
    READ(*,*) dt
    PRINT '("Enter start of fit")'
    READ(*,*) startf
    PRINT '("Enter end of fit")'
    READ(*,*) endf

    nframes = nsteps + 1

    CALL readtrr(filename,nframes,nmols,nsites,input)

    CALL einstein (nsteps,nframes,nmols,nsites,dt,input,startf,endf,dcoeff)

    PRINT*, "The diffusion constant is", dcoeff, "cm^2/s"

    !DO loop for printing out data extracted from trr

    !OPEN(2, FILE = "test.xvg", IOSTAT = ios, STATUS = "UNKNOWN")

    !DO i=1,3 
        !DO j=1, nmols
                
            !DO k=1, nsites

                !WRITE(2,*,IOSTAT = ios) input(i,j,k,1), input(i,j,k,2), input(i,j,k,3)

            !END DO

        !END DO   
        !WRITE(2,*,IOSTAT = ios) 
    !END DO
    !CLOSE(2)

    DEALLOCATE(input)

END PROGRAM diffusion