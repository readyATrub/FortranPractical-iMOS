MODULE myfunction

private
public :: e

contains

FUNCTION  e (x) result(y)
IMPLICIT NONE 
real, INTENT(IN) :: x
real :: y
y = exp(x)
END FUNCTION e 

END MODULE myfunction


