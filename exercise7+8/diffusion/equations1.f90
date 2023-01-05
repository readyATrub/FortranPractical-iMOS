MODULE equations

    PRIVATE 
    PUBLIC :: einstein

    CONTAINS

    SUBROUTINE einstein (nsteps,nframes,nmols,nsites,dt,input,startf,endf,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nsteps, nframes,nmols,nsites
        REAL, INTENT(IN) :: dt,startf,endf
        REAL, DIMENSION(1:nframes,1:nmols,1:nsites,1:3), INTENT(IN) :: input
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: ratom
        REAL :: msd,dcoeff
        INTEGER :: i,j,k,startstep,endstep
        
        ALLOCATE(ratom(1:2,1:nmols,1:nsites))

        startstep = (startf/dt)+1
        endstep = (endf/dt)+1
        msd = 0.0

        DO j=1, nmols

            DO k=1, nsites

            ratom(1,j,k) = sqrt(input(startstep,j,k,1)**2+input(startstep,j,k,2)**2+input(startstep,j,k,3)**2)
            ratom(2,j,k) = sqrt(input(endstep,j,k,1)**2+input(endstep,j,k,2)**2+input(endstep,j,k,3)**2)
            
            msd = msd + ABS(ratom(2,j,k)-ratom(1,j,k))**2.0

            END DO

        END DO

        msd = msd/(nmols*nsites)

        dcoeff = (msd/(2.0*dt*(nsteps)))*10.0**(-2.0)

        DEALLOCATE(ratom) 

    END SUBROUTINE einstein

END MODULE equations