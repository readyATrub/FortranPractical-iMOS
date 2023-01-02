PROGRAM mproduct
    USE matrix
    USE system 
    IMPLICIT NONE
    
    INTEGER :: i,j,n, memory
    CHARACTER :: s
    REAL :: tr, start, end, tra, trb
    REAL, DIMENSION(:,:), ALLOCATABLE:: a,b,c

    WRITE(*,*) "Side length of the to be generated matrices:"
    READ(*,*) n
    ALLOCATE(a(1:n,1:n),b(1:n,1:n),c(1:n,1:n))
    WRITE(*,*) "Method of matrix product computation &
    (1 for self-written function) (2 for DGEMM function) &
    (3 for Fortran 90 intrinsic matrix product function) &
    (4 for no matrix product):"
    READ(*,*) s
    
    IF (s=="1") THEN
        CALL CPU_TIME(start)
        a = makea(n)
        b = makeb(n)
        c = multiplicate(a,b,n)
        CALL CPU_TIME(end)
        
        tr = 0
        DO i=1, n
            tr = c(i,i) + tr 
        END DO
        WRITE(*,*) "The first element of the matrix product &
        is",c(1,1),"and its trace equals to",tr
        WRITE(*,*) "Elapsed CPU time:",end-start,"s"
    ELSE IF (s=="2") THEN
        CALL CPU_TIME(start)
        a = makea(n)
        b = makeb(n)
        CALL DGEMM ('n','n',n,n,n,1.0,a,n,b,n,1.0,c,n)
        CALL CPU_TIME(end)

        tr = 0
        DO i=1, n
            tr = c(i,i) + tr 
        END DO
        WRITE(*,*) "The first element of the matrix product &
        is",c(1,1),"and its trace equals to",tr
        WRITE(*,*) "Elapsed CPU time:",end-start,"s"
    ELSE IF (s=="3") THEN
        CALL CPU_TIME(start)
        a = makea(n)
        b = makeb(n)
        c = matmul(a,b)
        CALL CPU_TIME(end)

        tr = 0
        DO i=1, n
            tr = c(i,i) + tr 
        END DO
        WRITE(*,*) "The first element of the matrix product &
        is",c(1,1),"and its trace equals to",tr
        WRITE(*,*) "Elapsed CPU time:",end-start,"s"

    ELSE IF (s=="4") THEN
        CALL CPU_TIME(start)
        a = makea(n)
        b = makeb(n)
        CALL dummy(a,b,n)
        CALL CPU_TIME(end)
        
        tra = 0
        trb = 0
        DO i=1, n
            tra = a(i,i) + tra
            trb = a(i,i) + trb
        END DO

        CALL system_mem_usage(memory)
        memory = memory/1000
        WRITE(*,*) "Elapsed CPU time:",end-start,"s - Used Memory:", memory,"MB"
        
        WRITE(*,*) "The first element of the matrix a is",a(1,1),&
        "and its trace equals to", tra
        WRITE(*,*) "The first element of the matrix b is",b(1,1),&
        "and its trace equals to", trb
    ELSE
        WRITE(*,*) "No method for matrix product computation chosen&
        . Program aborted!"
    END IF

    DEALLOCATE(a,b,c)

END PROGRAM