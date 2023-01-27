MODULE integrator

    USE potential
    PRIVATE
    PUBLIC :: euler1, euler2, velv, euler3, ruku4

    CONTAINS

    SUBROUTINE euler1 (x0,v0,dt,m,x,v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v

        x = x0 + v0*dt
        v = v0 + (f(x0)/m) * dt

    END SUBROUTINE euler1

    SUBROUTINE euler2 (x0,v0,dt,m,x,v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v

        x = x0 + v0*dt + 0.5 * (f(x0)/m) * (dt**2.0)
        v = v0 + (f(x0)/m) * dt + 0.5 * ((df(x0) * v0)/m) * (dt**2.0)

    END SUBROUTINE euler2
    
    SUBROUTINE euler3 (x0,v0,dt,m,x,v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v

        x = x0 + v0*dt + 0.5 * (f(x0)/m) * dt**2.0 + (1.0/6.0) * (df(x0) * v0)/(m) * dt**3.0
        v = v0 + (f(x0)/m) * dt + 0.5 * (df(x0) * v0)/m * dt**2 +  (1.0/6.0)* &
        ((df2(x0) * v0**2)/m + (df(x0) * (f(x0)/m))/(m)) * dt**3.0

    END SUBROUTINE euler3

    SUBROUTINE velv (x0,v0,dt,m,x,v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v

        x = x0 + v0*dt + 0.5 * (f(x0)/m) * dt**2.0
        v = v0 + 0.5 * ((f(x0)/m)+(f(x)/m)) * dt

    END SUBROUTINE velv
END MODULE