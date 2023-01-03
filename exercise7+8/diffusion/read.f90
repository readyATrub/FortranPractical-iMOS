MODULE read
    
    PRIVATE
    PUBLIC :: readtrr

    CONTAINS 

    SUBROUTINE readtrr (filename,nframes,nmols,nsites,input)
        IMPLICIT NONE
        CHARACTER(LEN=200), INTENT(IN) :: filename
        CHARACTER(LEN=4) :: SOL,site
        INTEGER, INTENT(IN):: nframes,nmols,nsites
        INTEGER :: i,j,k,ios,index
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: input

    
        ALLOCATE(input(1:nmols,1:nsites,1:3))

        OPEN(1, FILE = filename, IOSTAT = ios, STATUS = "OLD")


            READ(1,IOSTAT = ios) 
            READ(1,IOSTAT = ios) 

            DO j=1, nmols
                
                DO k=1, nsites

                    READ(1,IOSTAT = ios) SOL, site, index, input(j,k,1), input(j,k,2), input(j,k,3)

                END DO

            END DO

            READ(1,IOSTAT = ios)

        CLOSE(1)
        


    END SUBROUTINE readtrr

    

    
END MODULE read