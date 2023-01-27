PROGRAM diffusion
    USE read
    USE equations
    IMPLICIT NONE

    CHARACTER(LEN=200) :: filename
    CHARACTER(LEN=1) :: s
    INTEGER :: nsteps, nframes, nmols, nsites,i,j,k,ios
    REAL :: dt,dcoeff,startf,endf,deltatres
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: inputr, inputv
    
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
    PRINT '("Enter start of fit:")'
    READ(*,*) startf
    PRINT '("Enter end of fit:")'
    READ(*,*) endf
    PRINT '("Enter Resolution of delta t:")'
    READ (*,*) deltatres

    nframes = nsteps + 1

    SELECT CASE(s)
        CASE("1")

            CALL readtrr(filename,nframes,nmols,nsites,inputr,inputv)
            CALL einstein(nsteps,nframes,nmols,nsites,&
            dt,inputr,startf,endf,deltatres,dcoeff)
            PRINT*, "The diffusion constant is", dcoeff, "cm^2/s"

        CASE("2")

            CALL readtrr(filename,nframes,nmols,nsites,inputr,inputv)
            CALL greenkubo(nsteps,nframes,nmols,nsites,&
            dt,inputv,startf,endf,deltatres,dcoeff)
            PRINT*, "The diffusion constant is", dcoeff, "cm^2/s"

        CASE DEFAULT 

            PRINT '("No method of computation chosen. Program aborted!")'
        
    END SELECT

END PROGRAM diffusion