MODULE read
    
    PRIVATE
    PUBLIC :: readtrr

    CONTAINS 

    SUBROUTINE readtrr (filename, input, numl, *)
        CHARACTER(LEN=200), INTENT(IN) :: filename
        CHARACTER(LEN=200) :: line
        INTEGER :: numl, ios, b, e, i
        REAL, DIMENSION(:,:), ALLOCATABLE :: input
        LOGICAL :: ifxst, ifat, ifhash

    

        INQUIRE(file=filename,exist=ifxst)
        IF(.not.ifxst) THEN
            WRITE(*,*) "File does not exist. Program aborted!"
            RETURN 1
        END IF
        
        OPEN(1, file=filename, status="OLD")
        b = 0

        DO
            READ(1, '(a)', IOSTAT=ios) line
            IF(ios > 0) THEN
                WRITE(*,*) "Reading file failed. Program aborted!"
                RETURN 1
            ELSE IF (ios < 0) THEN
                WRITE(*,*) "File has no data. Program aborted!"
                RETURN 1
            END IF

            ifat = line(1:1)/="@"
            ifhash = line(1:1)/="#"
            IF(ifhash.AND.ifat) EXIT
            b = b + 1
        END DO

        e = b

        DO
            e = e + 1
            READ(1,*,  IOSTAT=ios) line
            IF (ios<0) EXIT
        END DO
        CLOSE(1)
        
        numl = e - b
        
        ALLOCATE(input(1:numl,2))

        OPEN(2, file=filename, status="OLD")
        
        DO i=1, b
            READ(2,*) line
        END DO

        DO i=1, numl
            READ(2,*) input(i,1), input(i,2)
        END DO
        CLOSE(2)


    END SUBROUTINE readtrr

    

    
END MODULE read