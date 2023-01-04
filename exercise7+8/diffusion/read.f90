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
        REAL :: velx, vely, velz
        REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: input


    
        ALLOCATE(input(1:nframes,1:nmols,1:nsites,1:3))

        OPEN(1, FILE = filename , STATUS = "OLD")
            
        DO i=1, nframes
            
            READ(1,*,IOSTAT = ios) 
            READ(1,*,IOSTAT = ios)

            DO j=1, nmols
                
                DO k=1, nsites

                    READ(1,*,IOSTAT = ios) SOL, site, index, &
                    input(i,j,k,1), input(i,j,k,2), input(i,j,k,3), velx, vely, velZ

                END DO

            END DO

            READ(1,*,IOSTAT = ios)
        
        END DO

        CLOSE(1)
        
    END SUBROUTINE readtrr

END MODULE read