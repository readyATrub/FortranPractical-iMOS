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
        v = v0 + (f(x0)/m) * dt + 0.5 * ((tdf(x0) * v0)/m) * (dt**2.0)

    END SUBROUTINE euler2
    
    SUBROUTINE euler3 (x0,v0,dt,m,x,v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v

        x = x0 + v0*dt + 0.5 * (f(x0)/m) * dt**2.0 + (1.0/6.0) * (tdf(x0) * v0)/(m) * dt**3.0
        v = v0 + (f(x0)/m) * dt + 0.5 * (tdf(x0) * v0)/m * dt**2 +  (1.0/6.0)* &
        ((tdf2(x0) * v0**2)/m + (tdf(x0) * (f(x0)/m))/(m)) * dt**3.0

    END SUBROUTINE euler3

    SUBROUTINE velv (x0,v0,dt,m,x,v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v

        x = x0 + v0*dt + 0.5 * (f(x0)/m) * dt**2.0
        v = v0 + 0.5 * ((f(x0)/m)+(f(x)/m)) * dt

    END SUBROUTINE velv

    SUBROUTINE ruku4 (x0,v0,dt,m,x,v)
        IMPLICIT NONE 
        REAL, INTENT(IN) :: x0, v0, dt, m
        REAL :: x, v
        REAL, DIMENSION(1:2) :: fq, k1, k2, k3, k4, q0, q
        
        fq(1) = v0
        fq(2) = (f(x0)/m)
        q0(1) = x0
        q0(2) = v0

        k1 = dt * fq
        k2 = dt * (fq + 0.5 * k1)
        k3 = dt * (fq + 0.5 * k2)
        k4 = dt * (fq + k3)

        q = q0 + (k1)/6.0 + (k2)/3.0 + (k3)/3.0 +(k4)/6.0

        x = q(1)
        v = q(2)
        
    END SUBROUTINE ruku4
END MODULE