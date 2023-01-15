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
        CALL system_mem_usage(memory)
        memoryr = memory/1000.0

        tra = trace(a,n)
        trb = trace(b,n)
        
    ELSE
        PRINT '("No method for matrix product computation chosen&
        . Program aborted!")'
    END IF

    IF(s=="4") THEN

        PRINT '("Elapsed CPU time:",f10.4,x,"s -Used Memory:",f10.2,x,"MB")',end-start,memoryr
        PRINT '("The first element of matrix a is",f10.4,x,&
        "and its trace equals to",f10.4,x)',a(1,1),tra
        PRINT '("The first element of matrix b is",f10.4,x,&
        "and its trace equals to",f10.4,x)',b(1,1),trb

    ELSE

        PRINT '("The first element of the matrix product is",f10.4,x,&
        "and its trace equals to",f10.4,x)',c(1,1),tr
        PRINT '("Elapsed CPU time:",f10.4,x,"s")',end-start

    END IF


    DEALLOCATE(a,b,c)

END PROGRAM