PROGRAM aprrox
    IMPLICIT none
    !Declaration of variables   
    INTEGER :: i, numl
    REAL :: x, y, x1, x2, y1, y2
    CHARACTER(LEN=100) :: filename
    REAL, DIMENSION(:,:), ALLOCATABLE :: table

    !Reading the input table with the number of lines being speciied
    !in the header (numl) and the variable x
    WRITE(*,*) "Name of your table file"
    READ(*,*) filename
    OPEN(1, file =filename, status="unknown")
    READ(1,*) numl
    ALLOCATE(table(1:numl,1:numl))
    READ(1,*) (table(i,1) , table(i,2), i =1, numl)
    CLOSE(1)
    WRITE(*,*) "Table file succesfully loaded!"
    WRITE(*,*) "Value of x for the approximated function value:"
    READ(*,*) x

    !Checking whether the x is within the range of the input table
    IF (table(1,1) >= x) THEN
        WRITE(*,*) "X-value is to small to be within the range of the input table. Program aborted!"
    ELSE IF (x>table(numl,1)) THEN
        WRITE(*,*) "X-value is to large to be within the range of the input table. Program aborted!"
    ELSE
    !Computation of the approximated function value of x (y) via
    !linear fit between the neighbouring x-values x1 and x2 of
    !the input table and printing the the result into the console
        i = 1
        DO
            IF (table(i,1) >= x) THEN
                y2= table(i,2)
                y1= table(i-1,2)
                x2= table(i,1)
                x1= table(i-1,1)
                y = ((y2-y1)/(x2-x1))*(x-x1)+y1
                EXIT
            END IF
            IF (numl==i) EXIT
            i = i+1
        END DO
        WRITE(*,*) "The approximated function value of",x,"is",y
    END IF
END PROGRAM aprrox