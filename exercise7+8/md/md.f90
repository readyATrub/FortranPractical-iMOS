PROGRAM md
    USE integrator
    USE potential
    IMPLICIT NONE

    REAL :: x0, v0, dt, m, x, v, t, epot, ekin, etot
    INTEGER :: i,n
    CHARACTER(LEN=1) :: s

    WRITE(*,*) "Mass of particle:"
    READ(*,*) m
    WRITE(*,*) "Initial position of particle:"
    READ(*,*) x0
    WRITE(*,*) "Initial velocity of particle:"
    READ(*,*) v0
    WRITE(*,*) "Time step size:"
    READ(*,*) dt
    WRITE(*,*) "Number of time steps:"
    READ(*,*) n
    WRITE(*,*) "Choose integrator:"
    WRITE(*,*) "(1 for 1st order Euler)"
    WRITE(*,*) "(2 for 2nd order Euler)"
    WRITE(*,*) "(3 for 3rd order Euler)"
    WRITE(*,*) "(4 for velocity-verlet)"
    READ(*,*) s


    
    IF(s=="1") THEN

        WRITE(*,*) "Processing MD-run for",dt*n,"time units over",n,"time steps." 
        i=0
        t = 0.0
        epot = pot(x0)
        ekin = 0.5*m*v0**2.0
        etot = epot + ekin
        
        OPEN(1, file="output_euler1.out", status="unknown")
        WRITE(1,*) i, t, x0, v0, etot, ekin, epot

        DO i=1, n
            CALL euler1 (x0,v0,dt,m,x,v)
            epot = pot(x)
            ekin = 0.5*m*v**2.0
            etot = epot + ekin
            t = t + dt

            WRITE(1,*) i, t, x, v, etot, ekin, epot

            x0 = x
            v0 = v
        END DO
        CLOSE(1)

        WRITE(*,*) "MD-run successfuly completed!"
    ELSE IF (s=="2") THEN
        
        WRITE(*,*) "Processing MD-run for",dt*n,"time units over",n,"time steps." 
        i=0
        t = 0.0
        epot = pot(x0)
        ekin = 0.5*m*v0**2.0
        etot = epot + ekin
        
        OPEN(1, file="output_euler2.out", status="unknown")
        WRITE(1,*) i, t, x0, v0, etot, ekin, epot

        DO i=1, n
            CALL euler2 (x0,v0,dt,m,x,v)
            epot = pot(x)
            ekin = 0.5*m*v**2.0
            etot = epot + ekin
            t = t + dt

            WRITE(1,*) i, t, x, v, etot, ekin, epot

            x0 = x
            v0 = v
        END DO
        CLOSE(1)

        WRITE(*,*) "MD-run successfuly completed!"
    ELSE IF (s=="3") THEN
        WRITE(*,*) "Processing MD-run for",dt*n,"time units over",n,"time steps." 
        i=0
        t = 0.0
        epot = pot(x0)
        ekin = 0.5*m*v0**2.0
        etot = epot + ekin
        
        OPEN(1, file="output_euler3.out", status="unknown")
        WRITE(1,*) i, t, x0, v0, etot, ekin, epot

        DO i=1, n
            CALL euler3 (x0,v0,dt,m,x,v)
            epot = pot(x)
            ekin = 0.5*m*v**2.0
            etot = epot + ekin
            t = t + dt

            WRITE(1,*) i, t, x, v, etot, ekin, epot

            x0 = x
            v0 = v
        END DO
        CLOSE(1)

        WRITE(*,*) "MD-run successfuly completed!"
    ELSE IF (s=="4") THEN
        WRITE(*,*) "Processing MD-run for",dt*n,"time units over",n,"time steps." 
        i=0
        t = 0.0
        epot = pot(x0)
        ekin = 0.5*m*v0**2.0
        etot = epot + ekin
        
        OPEN(1, file="output_velv.out", status="unknown")
        WRITE(1,*) i, t, x0, v0, etot, ekin, epot

        DO i=1, n
            CALL velv (x0,v0,dt,m,x,v)
            epot = pot(x)
            ekin = 0.5*m*v**2.0
            etot = epot + ekin
            t = t + dt

            WRITE(1,*) i, t, x, v, etot, ekin, epot

            x0 = x
            v0 = v
        END DO
        CLOSE(1)

        WRITE(*,*) "MD-run successfuly completed!"
    ELSE 
    WRITE(*,*) "No integrator was chosen. Program aborted!"
    END IF

    
END PROGRAM md