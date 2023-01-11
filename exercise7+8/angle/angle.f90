PROGRAM angle

IMPLICIT none

INTEGER :: i, dim
REAL, DIMENSION(:),ALLOCATABLE :: vector1, vector2
REAL :: scalarproduct, x 
REAL :: norm1, norm2
REAL :: phi
REAL, PARAMETER :: pi = acos(-1.0)

WRITE(*,*) "Dimension of your vectors:"
READ(*,*) dim
ALLOCATE(vector1(1:dim),vector2(1:dim))
WRITE(*,*) "Elements of your first vector:"
READ(*,*) vector1
WRITE(*,*) "Elements of your second vector:"
READ(*,*) vector2

DO i = 1, dim

x = vector1(i) * vector2(i)
scalarproduct = x + scalarproduct

x = (vector1(i))**2
norm1 = x + norm1

x = (vector2(i))**2
norm2 = x + norm2

END DO

norm1 = sqrt(norm1)
norm2 = sqrt(norm2)

phi = acos(scalarproduct/(norm1*norm2))/(2*pi) * 360

Write(*,*) "The angle between both vectors is",phi,"degrees"

END PROGRAM angle
