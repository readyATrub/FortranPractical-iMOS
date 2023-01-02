MODULE potential
    PRIVATE
    PUBLIC :: pot, f, tdf, tdf2

    CONTAINS 

    FUNCTION pot (x) result(v)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL :: v
        v = 4.0*(x**(-12.0)-x**(-6.0))
    END FUNCTION pot

    FUNCTION f (x) result(force)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL :: force
        force = -24.0*((-2.0/x**(13.0))+(1.0/x**(7.0)))
    END FUNCTION f

    FUNCTION tdf (x) result(tiderivf)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL :: tiderivf
        tiderivf = -24.0*((26.0/x**(14.0))-(7/x**(8.0)))
    END FUNCTION tdf

    FUNCTION tdf2 (x) result(tiderivf2)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL :: tiderivf2
        tiderivf2 = -672.0*((-13.0/x**(15.0))+(2/x**(9.0)))
    END FUNCTION tdf2

END MODULE potential