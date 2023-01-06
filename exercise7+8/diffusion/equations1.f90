MODULE equations

    PRIVATE 
    PUBLIC :: einstein, greenkubo

    CONTAINS

    SUBROUTINE einstein (nsteps,nframes,nmols,nsites,dt,inputr,startf,endf,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nsteps, nframes,nmols,nsites
        REAL, INTENT(IN) :: dt,startf,endf
        REAL, DIMENSION(1:nframes,1:nmols,1:nsites,1:3), INTENT(IN) :: inputr
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: ratom
        REAL :: msd,dcoeff
        INTEGER :: i,j,k,startstep,endstep
        
        ALLOCATE(ratom(1:2,1:nmols,1:nsites))

        startstep = (startf/dt)+1
        endstep = (endf/dt)+1
        msd = 0.0

        DO j=1, nmols

            DO k=1, nsites

            ratom(1,j,k) = sqrt(inputr(startstep,j,k,1)**2+inputr(startstep,j,k,2)**2+inputr(startstep,j,k,3)**2)
            ratom(2,j,k) = sqrt(inputr(endstep,j,k,1)**2+inputr(endstep,j,k,2)**2+inputr(endstep,j,k,3)**2)
            
            msd = msd + ABS(ratom(2,j,k)-ratom(1,j,k))**2.0

            END DO

        END DO

        msd = msd/(nmols*nsites)

        dcoeff = (msd/(2.0*dt*(nsteps)))*10.0**(-2.0)

        DEALLOCATE(ratom) 

    END SUBROUTINE einstein

    SUBROUTINE greenkubo (nsteps,nframes,nmols,nsites,dt,inputv,startf,endf,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nsteps, nframes,nmols,nsites
        REAL, INTENT(IN) :: dt,startf,endf
        REAL, DIMENSION(1:nframes,1:nmols,1:nsites,1:3), INTENT(IN) :: inputv
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: vatom
        REAL, DIMENSION(:), ALLOCATABLE :: mp
        REAL :: dcoeff
        INTEGER :: i,j,k,startstep,endstep
        
        startstep = (startf/dt)+1
        endstep = (endf/dt)+1
        ALLOCATE(vatom(1:2,1:nmols,1:nsites),mp(1:nframes))

        dcoeff = 0.0

        DO i=1, nframes
            
            mp(i) = 0.0

            DO j=1, nmols

                DO k=1, nsites

                    !vatom(1,j,k) = sqrt(inputv(1,j,k,1)**2+inputv(1,j,k,2)**2+inputv(1,j,k,3)**2)
                    !vatom(2,j,k) = sqrt(inputv(i,j,k,1)**2+inputv(i,j,k,2)**2+inputv(i,j,k,3)**2)
            
                    !mp(i) = mp(i) + vatom(1,j,k)*vatom(2,j,k)
                    mp(i) = mp(i) + sqrt(inputv(1,j,k,1)**2+inputv(1,j,k,2)**2+inputv(1,j,k,3)**2) &
                    * sqrt(inputv(i,j,k,1)**2+inputv(i,j,k,2)**2+inputv(i,j,k,3)**2)

                    !mp(i) = mp(i) + ABS(inputv(1,j,k,1)*inputv(i,j,k,1))+&
                    !ABS(inputv(1,j,k,2)*inputv(i,j,k,2))+ABS(inputv(1,j,k,3)*inputv(i,j,k,3))


                END DO

            END DO

        mp(i) = mp(i)/(nmols*nsites)

        dcoeff = dcoeff + mp(i) * dt

        END DO

        dcoeff = (1.0/3.0)*dcoeff*10**(-2.0)

        DEALLOCATE(vatom,mp) 

    END SUBROUTINE greenkubo

END MODULE equations