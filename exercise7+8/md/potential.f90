MODULE potential
    PRIVATE
    PUBLIC :: pot, f, df, df2

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
        force = -1.0*((-48.0*(x**(-13.0)))+(24.0*(x**(-7.0))))
    END FUNCTION f

    FUNCTION df (x) result(derivf)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL :: derivf
        derivf = (-4*(((12*13)/(x**14))-((6*7)/(x**8))))
    END FUNCTION df

    FUNCTION df2 (x) result(derivf2)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL :: derivf2
        derivf2 = -672.0*((-13.0/x**(15.0))+(2/x**(9.0)))
    END FUNCTION df2

END MODULE potential