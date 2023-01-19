MODULE system

PRIVATE 
PUBLIC ::  system_mem

CONTAINS

SUBROUTINE system_mem(memory)
    IMPLICIT NONE
    INTEGER :: memory, pid
    CHARACTER(LEN=200):: filename
    CHARACTER(LEN=80) :: line
    CHARACTER(LEN=8)  :: pidchar
    logical :: ifxst
    
    memory=-1
    
    pid=getpid()
    WRITE(pidchar,'(I8)') pid
    filename='/proc/'//trim(adjustl(pidchar))//'/status'
    
    INQUIRE (file=filename,exist=ifxst)
    IF (.not.ifxst) THEN
      WRITE (*,*) 'System file does not exist.'
      RETURN
    END IF
    
    OPEN(100, file=filename, action='read')

    DO
      READ (100,'(a)',end=120) line
      IF (line(1:6).eq.'VmRSS:') then
         READ (line(7:),*) memory
         EXIT
      END IF
    END DO

    120 CONTINUE
    CLOSE(100)
    
    RETURN
  END SUBROUTINE system_mem

END MODULE system