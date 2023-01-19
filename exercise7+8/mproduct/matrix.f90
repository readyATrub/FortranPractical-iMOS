MODULE matrix
    PRIVATE
    PUBLIC :: makematrices, multiplicate, trace, dummy

    contains
    SUBROUTINE makematrices (a,b,n)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        INTEGER :: i,j
        REAL :: nr
        REAL(kind=8), PARAMETER :: pi= acos(-1.0)
        REAL(kind=8), DIMENSION(:,:), ALLOCATABLE:: a,b

        ALLOCATE(a(1:n,1:n),b(1:n,1:n))
        nr = n
        
        DO i=1, n 
            DO j=i, n 
                a(i,j)= sin(2*pi*i*j/nr)
                a(j,i)= sin(2*pi*i*j/nr) 
                b(i,j)= cos(2*pi*i*j/nr)
                b(j,i)= cos(2*pi*i*j/nr)
             END DO
        END DO
    END SUBROUTINE makematrices

    FUNCTION multiplicate (a,b,n) result (c)
        IMPLICIT NONE
        INTEGER :: i,j,k
        INTEGER, INTENT(IN) :: n
        REAL(kind=8), DIMENSION(1:n,1:n), INTENT(IN):: a,b
        REAL(kind=8), DIMENSION(1:n,1:n) :: c
        DO i=1, n
            DO j=1, n
                c(i,j)=0
                DO k=1, n
                c(i,j) = c(i,j) + a(i,k) * b(k,j)
                END DO
            END DO    
        END DO
    END FUNCTION multiplicate

    FUNCTION trace(a,n) result (tr)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        INTEGER :: i
        REAL(kind=8) :: tr
        REAL(kind=8), DIMENSION(1:n,1:n), INTENT(IN):: a

        tr = 0
        DO i=1, n
            tr = tr + a(i,i)
        END DO
    
    END FUNCTION trace

    SUBROUTINE dummy (a,b,n)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        REAL(kind=8), DIMENSION(1:n,1:n), INTENT(IN):: a,b
    END SUBROUTINE dummy
END MODULE matrix