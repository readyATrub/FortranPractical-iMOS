PROGRAM diffusion
    USE read
    IMPLICIT NONE

    CHARACTER(LEN=200) :: filename, line
    INTEGER :: numl, i, b, e
    REAL, DIMENSION(:,:), ALLOCATABLE :: input
    
    WRITE(*,*) "Enter trajectory file name"
    READ(*,*) filename

    CALL readtrr(filename, input, numl, *100)

    OPEN(3, file="test.xvg", status="UNKNOWN")
    DO i=1, numl
        WRITE(3,*) input(i,1), input(i,2)
    END DO
    CLOSE(15)

    100 CONTINUE

END PROGRAM diffusion