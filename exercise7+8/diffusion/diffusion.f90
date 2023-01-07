PROGRAM diffusion
    USE read
    USE equations
    IMPLICIT NONE

    CHARACTER(LEN=200) :: filename
    CHARACTER(LEN=1) :: s
    INTEGER :: nsteps, nframes, nmols, nsites,i,j,k,ios
    REAL :: dt,dcoeff,startf,endf
    REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: inputr, inputv
    
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
    PRINT '("Enter method of computation:")'
    PRINT '("(1 for Einstein)")'
    PRINT '("(2 for Green-Kubo)")'
    READ(*,*) s

    nframes = nsteps + 1

    SELECT CASE(s)
        CASE("1")

            PRINT '("Enter start of fit")'
            READ(*,*) startf
            PRINT '("Enter end of fit")'
            READ(*,*) endf

            CALL readtrr(filename,nframes,nmols,nsites,inputr,inputv)
            CALL einstein (nsteps,nframes,nmols,nsites,dt,inputr,startf,endf,dcoeff)
            PRINT*, "The diffusion constant is", dcoeff, "cm^2/s"

        CASE("2")

            CALL readtrr(filename,nframes,nmols,nsites,inputr,inputv)
            CALL greenkubo (nframes,nmols,nsites,dt,inputv,dcoeff)
            PRINT*, "The diffusion constant is", dcoeff, "cm^2/s"

        CASE DEFAULT 

            PRINT '("No method of computation chosen. Program aborted!")'
        
    END SELECT
    
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

    DEALLOCATE(inputr,inputv)

END PROGRAM diffusion