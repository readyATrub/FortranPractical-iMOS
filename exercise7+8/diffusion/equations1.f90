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

    SUBROUTINE greenkubo (nframes,nmols,nsites,dt,inputv,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nframes,nmols,nsites
        REAL, INTENT(IN) :: dt
        REAL, DIMENSION(1:nframes,1:nmols,1:nsites,1:3), INTENT(IN) :: inputv
        REAL, DIMENSION(:), ALLOCATABLE :: mp
        REAL :: dcoeff
        INTEGER :: i,j,k,l,ios
    
        ALLOCATE(mp(1:nframes))

        dcoeff = 0.0

        DO i=1, nframes
            
            mp(i) = 0.0

            DO j=1, nmols

                DO k=1, nsites

                    DO l=1, 3

                        mp(i) = mp(i) + inputv(1,j,k,l)*inputv(i,j,k,l)

                    END DO

                END DO
            
            END DO

            mp(i) = mp(i)/(nmols*nsites)
            dcoeff = dcoeff + (mp(i)/mp(1)) * dt 

        END DO

        dcoeff = (1.0/3.0)*dcoeff*10**(-2.0) 

        !Routine for printing out normalized velocity autocorelation function
        !OPEN(2, FILE = "velacc.dat", IOSTAT = ios, STATUS = "UNKNOWN")

        !DO i=1 , nsteps, 1
            !WRITE(2,*,IOSTAT = ios) dt*(i-1), mp(i)/mp(1)

        !END DO

        !CLOSE(2)

        DEALLOCATE(mp) 

    END SUBROUTINE greenkubo

END MODULE equations