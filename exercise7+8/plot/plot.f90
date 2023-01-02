PROGRAM plot
USE myfunction
IMPLICIT none

INTEGER :: i, n
REAL :: x, dx
REAL, DIMENSION(:,:), ALLOCATABLE :: output

WRITE(*,*) "Starting value of x:"
READ(*,*) x
WRITE(*,*) "Step size dx:"
READ(*,*) dx
WRITE(*,*) "Number of steps:"
READ(*,*) n
ALLOCATE(output(1:n,1:n))

DO i = 1, n
output(i,1) = x
output(i,2) = e(x)
x = x + dx
END DO

open(1, file ="output.xvg", status="unknown")
DO i = 1, n
WRITE(1,*) output(i,1) , output(i,2)
END DO
CLOSE(1)

END PROGRAM plot
