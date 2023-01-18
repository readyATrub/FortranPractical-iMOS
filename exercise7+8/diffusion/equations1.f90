MODULE equations

    PRIVATE 
    PUBLIC :: einstein, greenkubo

    CONTAINS

    SUBROUTINE einstein (nsteps,nframes,nmols,nsites,dt,inputr,startf,endf,deltatres,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nsteps, nframes,nmols,nsites
        REAL, INTENT(IN) :: dt,startf,endf,deltatres
        REAL, DIMENSION(1:3,nmols*nsites,nframes), INTENT(IN) :: inputr
        REAL, DIMENSION(:), ALLOCATABLE :: msd
        REAL :: dcoeff,diff
        INTEGER :: i,j,k,l,startstep,endstep,stepdiffs,stepsize,deltat,ios

        startstep = (startf/deltatres)+1
        endstep = (endf/deltatres)+1
        stepdiffs = endstep-startstep
        stepsize= (deltatres/dt)
        ALLOCATE(msd(1:(nsteps/stepsize)+1))
        deltat = 0.0
        dcoeff = 0.0

        OPEN(3, FILE = "MSD.xvg", IOSTAT = ios, STATUS = "UNKNOWN")
        DO i = 1, (nframes/stepsize)+1
        
            msd(i) = 0
            DO j = 1, nframes-deltat

                diff= 0.0

                DO k = 1, nmols*nsites

                    DO l = 1, 3
                 
                        diff = ABS(inputr(l,k,j+deltat) - inputr(l,k,j))**(2.0) +diff

                    END DO
                        
                END DO
                msd(i) = msd(i)+diff/(nmols*nsites)

                WRITE(*,*) i,j,deltat
            END DO

            msd(i) = msd(i)/(nframes-deltat)
            WRITE(3,*,IOSTAT = ios) deltat*dt, msd(i)
            deltat = deltat + stepsize

            
        END DO
        CLOSE(3)

        DO i=startstep, endstep
        dcoeff = dcoeff + msd(i)
        END DO
        dcoeff = dcoeff/stepdiffs

        dcoeff = (dcoeff/(6.0*(endf-startf)))*10.0**(-2.0)

        DEALLOCATE(msd)

    END SUBROUTINE einstein

    SUBROUTINE greenkubo (nsteps,nframes,nmols,nsites,dt,inputv,startf,endf,deltatres,dcoeff)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: nsteps, nframes,nmols,nsites
        REAL, INTENT(IN) :: dt,startf,endf,deltatres
        REAL, DIMENSION(1:3,nmols*nsites,nframes), INTENT(IN) :: inputv
        REAL, DIMENSION(:), ALLOCATABLE :: mp
        REAL :: dcoeff,p
        INTEGER :: i,j,k,l,startstep,endstep,stepdiffs,stepsize,deltat,ios

        startstep = (startf/deltatres)+1
        endstep = (endf/deltatres)+1
        stepdiffs = endstep-startstep
        stepsize= (deltatres/dt)+1
        ALLOCATE(mp(1:(nsteps/stepsize)+1))
        deltat = 0.0
        dcoeff = 0.0
        OPEN(4, FILE = "velacc.xvg", IOSTAT = ios, STATUS = "UNKNOWN")
        DO i = 1, (nframes/stepsize)+1
        
            mp(i) = 0
            DO j = 1, nframes-deltat

                p= 0.0

                DO k = 1, nmols*nsites

                    DO l = 1, 3
                 
                        p = inputv(l,k,j+deltat) * inputv(l,k,j) +p

                    END DO
                        
                END DO
                mp(i) = mp(i)+p/(nmols*nsites)
                WRITE(*,*) i,j
            END DO

            mp(i) = mp(i)/(nframes-deltat)
            WRITE(4,*,IOSTAT = ios) deltat*dt, mp(i)
            deltat = deltat + stepsize

            
        END DO
        CLOSE(4)

        DO i=startstep, endstep
        dcoeff = dcoeff + mp(i)/stepdiffs * deltatres
        END DO
        

        dcoeff = (dcoeff/3.0)*10.0**(-2.0)

    END SUBROUTINE greenkubo

END MODULE equations