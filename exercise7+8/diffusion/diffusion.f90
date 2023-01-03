PROGRAM diffusion
    USE read
    IMPLICIT NONE

    CHARACTER(LEN=200) :: filename
    INTEGER :: nframes, nmols, nsites,dim,j,k,ios
    REAL :: dt
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: input
    
    PRINT '("Enter trajectory file:")'
    READ(*,*) filename
    PRINT '("Enter number of water molecules:")'
    READ(*,*) nmols
    PRINT '("Enter number of sites:")'
    READ(*,*) nsites
    PRINT '("Enter number of frames:")'
    READ(*,*) nframes
    PRINT '("Enter time step size:")'
    READ(*,*) dt


    CALL readtrr(filename,nframes,nmols,nsites,input)

    OPEN(2, FILE = "test.xvg", IOSTAT = ios, STATUS = "UNKNOWN")

        
    DO j=1, nmols
                
        DO k=1, nsites

            WRITE(2,IOSTAT = ios) input(j,k,1), input(j,k,2), input(j,k,3)

        END DO

    END DO    
    CLOSE(2)



END PROGRAM diffusion