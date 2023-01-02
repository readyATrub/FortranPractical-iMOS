MODULE matrix
    PRIVATE
    PUBLIC :: makea, makeb, multiplicate, dummy

    contains
    FUNCTION makea (n) result(a)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        INTEGER :: i,j
        REAL :: nr
        REAL, PARAMETER :: pi= acos(-1.0)
        REAL, DIMENSION(1:n,1:n):: a

        nr = n
        
        DO i=1, n 
            DO j=1, n 
                a(i,j)= sin(2*pi*i*j/nr)    
             END DO
        END DO
    END FUNCTION makea

    FUNCTION makeb (n) result(b)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        INTEGER :: i,j
        REAL :: nr
        REAL, PARAMETER :: pi= acos(-1.0)
        REAL, DIMENSION(1:n,1:n):: b

        nr = n
        
        DO i=1, n 
            DO j=1, n 
                b(i,j)= cos(2*pi*i*j/nr)
            END DO
        END DO
    END FUNCTION makeb

    FUNCTION multiplicate (a,b,n) result (c)
        IMPLICIT NONE
        INTEGER :: i,j,k
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(1:n,1:n), INTENT(IN):: a,b
        REAL, DIMENSION(1:n,1:n) :: c
        DO i=1, n
            DO j=1, n
                c(i,j)=0
                DO k=1, n
                c(i,j) = c(i,j) + a(i,k) * b(k,j)
                END DO
            END DO    
        END DO
    END FUNCTION multiplicate

    SUBROUTINE dummy (a,b,n)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(1:n,1:n), INTENT(IN):: a,b
    END SUBROUTINE dummy
END MODULE matrix