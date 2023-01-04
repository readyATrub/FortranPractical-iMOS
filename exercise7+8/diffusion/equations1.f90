MODULE equations

    PRIVATE 
    PUBLIC :: einstein

    CONTAINS

    SUBROUTINE einstein (nframes,nmols,nsites,dt,input,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nframes,nmols,nsites
        REAL, INTENT(IN) :: dt
        REAL, DIMENSION(1:nframes,1:nmols,1:nsites,1:3), INTENT(IN) :: input
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: ratom
        REAL, DIMENSION(:,:), ALLOCATABLE :: rmol
        REAL :: msdev,dcoeff
        INTEGER :: i,j,k
        
        ALLOCATE(rmol(1:2,1:nmols),ratom(1:2,1:nmols,1:nsites))

        
        msdev = 0

        DO j=1, nmols

            rmol(1,j) = 0
            rmol(2,j) = 0

            DO k=1, nsites
            ratom(1,j,k) = sqrt(input(5000,j,k,1)**2+input(5000,j,k,2)**2+input(5000,j,k,3)**2)
            ratom(2,j,k) = sqrt(input(nframes,j,k,1)**2+input(nframes,j,k,2)**2+input(nframes,j,k,3)**2)

            rmol(1,j) = rmol(1,j) + ratom(1,j,k)
            rmol(2,j) = rmol(2,j) + ratom(2,j,k)

            END DO

            rmol(1,j) = rmol(1,j)/nsites
            rmol(2,j) = rmol(2,j)/nsites

            !rmol(1,j) = (ratom(1,j,2)+ratom(1,j,3))/2.0
            !rmol(2,j) = (ratom(2,j,2)+ratom(2,j,3))/2.0

            msdev = msdev + ABS(rmol(2,j)-rmol(1,j))**2


        END DO

        msdev = msdev/nmols

        dcoeff = msdev/(2.0*dt*(nframes-1.0))

        DEALLOCATE(ratom,rmol) 

    END SUBROUTINE einstein

END MODULE equations