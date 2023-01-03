PROGRAM diffusion
    USE read
    IMPLICIT NONE

    CHARACTER(LEN=200) :: filename
    INTEGER :: nframes, nmols, nsites,i,j,k,ios
    REAL :: dt
    REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: input
    
    PRINT '("Enter trajectory file (in quotation marks):")'
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

    !DO loop for printing out data extracted from trr

!DO i=1,3 
    !DO j=1, nmols
                
        !DO k=1, nsites

            !WRITE(2,*,IOSTAT = ios) input(i,j,k,1), input(i,j,k,2), input(i,j,k,3)

        !END DO

    !END DO   
    !WRITE(2,*,IOSTAT = ios) 
!END DO
    1CLOSE(2)

    DEALLOCATE(input)

END PROGRAM diffusion