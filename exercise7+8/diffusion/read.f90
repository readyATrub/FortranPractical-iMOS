MODULE read
    
    PRIVATE
    PUBLIC :: readtrr

    CONTAINS 

    SUBROUTINE readtrr (filename,nframes,nmols,nsites,inputr,inputv)
        IMPLICIT NONE
        CHARACTER(LEN=200), INTENT(IN) :: filename
        CHARACTER(LEN=4) :: SOL,site
        INTEGER, INTENT(IN):: nframes,nmols,nsites
        INTEGER :: i,j,k,ios,index
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: inputr, inputv


    
        ALLOCATE(inputr(1:3,nmols*nsites,nframes),&
        inputv(1:3,nmols*nsites,nframes))

        OPEN(1, FILE = filename , STATUS = "OLD")
            
        DO i=1, nframes
            
            READ(1,*,IOSTAT = ios) 
            READ(1,*,IOSTAT = ios)

            DO j=1, nmols*nsites
                

                    READ(1,*,IOSTAT = ios) SOL, site, index, &
                    inputr(1:3,j,i) ,inputv(1:3,j,i)
            END DO

            READ(1,*,IOSTAT = ios)
        
        END DO

        CLOSE(1)
        !CLOSE(2)
        
    END SUBROUTINE readtrr

END MODULE read