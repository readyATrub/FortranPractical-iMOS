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
        REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: inputr, inputv


    
        ALLOCATE(inputr(1:nframes,1:nmols,1:nsites,1:3),inputv(1:nframes,1:nmols,1:nsites,1:3))

        OPEN(1, FILE = filename , STATUS = "OLD")
            
        DO i=1, nframes
            
            READ(1,*,IOSTAT = ios) 
            READ(1,*,IOSTAT = ios)

            DO j=1, nmols
                
                DO k=1, nsites

                    READ(1,*,IOSTAT = ios) SOL, site, index, &
                    inputr(i,j,k,1), inputr(i,j,k,2), inputr(i,j,k,3), &
                    inputv(i,j,k,1), inputv(i,j,k,2), inputv(i,j,k,3)

                END DO

            END DO

            READ(1,*,IOSTAT = ios)
        
        END DO

        CLOSE(1)
        
    END SUBROUTINE readtrr

END MODULE read