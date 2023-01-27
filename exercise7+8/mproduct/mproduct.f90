PROGRAM mproduct
    USE matrix
    USE system 
    IMPLICIT NONE
    
    INTEGER :: i,j,n, memory
    CHARACTER :: s
    REAL :: tr, start, end, tra, trb, memoryr
    REAL(kind=8), DIMENSION(:,:), ALLOCATABLE:: a,b,c

    WRITE(*,*) "Side length of the to be generated matrices:"
    READ(*,*) n
    WRITE(*,*) "Method of matrix product computation &
    (1 for self-written function) (2 for DGEMM function) &
    (3 for Fortran 90 intrinsic matrix product function) &
    (4 for no matrix product):"
    READ(*,*) s

    ALLOCATE(c(1:n,1:n))
    CALL CPU_TIME(start)
    CALL makematrices (a,b,n)

    IF (s=="1") THEN

        c = multiplicate(a,b,n)
        CALL CPU_TIME(end)
        tr = trace(c,n)

    ELSE IF (s=="2") THEN

        CALL DGEMM ('n','n',n,n,n,1.0,a,n,b,n,1.0,c,n)
        CALL CPU_TIME(end)
        tr = trace(c,n)

    ELSE IF (s=="3") THEN

        c = matmul(a,b)
        CALL CPU_TIME(end)
        tr = trace(c,n)

    ELSE IF (s=="4") THEN

        CALL dummy(a,b,n)
        CALL CPU_TIME(end)
        CALL system_mem(memory)
        memoryr = memory/1000.0

        tra = trace(a,n)
        trb = trace(b,n)
        
    ELSE
        PRINT '("No method for matrix product computation chosen&
        . Program aborted!")'
    END IF

    IF(s=="4") THEN

        PRINT *, "Elapsed CPU time:",end-start,"s -Used Memory:",memoryr,"MB"
        PRINT *, "The first element of matrix a is",a(1,1),&
        "and its trace equals to",tra
        PRINT *,"The first element of matrix b is",b(1,1),&
        "and its trace equals to",trb

    ELSE

        PRINT *,"The first element of the computed matrix c is",c(1,1),&
        "and its trace equals to",tr
        PRINT *, "Elapsed CPU time:",end-start,"s"

    END IF


    DEALLOCATE(a,b,c)

END PROGRAM