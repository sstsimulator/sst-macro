! ************************************************************************
!
!               miniGhost: stencil computations with boundary exchange.
!                 Copyright (2011) Sandia Corporation
!
! Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
! license for use of this work by or on behalf of the U.S. Government.
!
! This library is free software; you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation; either version 2.1 of the
! License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
! USA
! Questions? Contact Richard F. Barrett (rfbarre@sandia.gov) or
!                    Michael A. Heroux (maherou@sandia.gov)
!
! ************************************************************************

MODULE MG_PACK_1DIR_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! This for packing EAST/WEST _or_ NORTH/SOUTH _or_ BACK/FRONT. Staged in this 
   ! manner to include diagonal entries. For simplicity, we include the ghost space 
   ! for each direction even though not needed for some exchanges.
   !
   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_PACK_1DIR ( DIR,  IERR )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         DIR            ! Direction of boundary exchange

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(IN) :: &
      !   GRID

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER ::        &
         I, J, K                         ! Counters

      REAL(KIND=MG_REAL8) ::      &
         TIME_START

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if !defined ( _MG_DEBUG )
      IF ( NUMPES == 1 ) &
         RETURN
#endif

      TIME_START = MPI_WTIME ( )

      IF ( DIR == DIR_BACK_FRONT ) THEN

         ! Back boundary
      
         IF ( NEIGHBORS(BACK) /= -1 ) THEN
            DO J = 0, NY+1
               DO I = 0, NX+1
                  COUNT_SEND_BACK = COUNT_SEND_BACK + 1
                 ! SEND_BUFFER_BACK ( COUNT_SEND_BACK ) = GRID ( I, J, 1 )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NX+1, 2)
         END IF
      
         ! Front boundary
      
         IF ( NEIGHBORS(FRONT) /= -1 ) THEN
            DO J = 0, NY+1
               DO I = 0, NX+1
                  COUNT_SEND_FRONT = COUNT_SEND_FRONT + 1
                 ! SEND_BUFFER_FRONT ( COUNT_SEND_FRONT ) = GRID ( I, J, NZ )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NX+1, 2)
         END IF
      
      ELSE IF ( DIR == DIR_EAST_WEST ) THEN

         ! East boundary
      
         IF ( NEIGHBORS(EAST) /= -1 ) THEN
             DO K = 0, NZ+1
               DO J = 0, NY+1
                  COUNT_SEND_EAST = COUNT_SEND_EAST + 1
                 ! SEND_BUFFER_EAST ( COUNT_SEND_EAST ) = GRID ( 1, J, K )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NZ+1, 2)
         END IF
      
         ! West boundary
      
         IF ( NEIGHBORS(WEST) /= -1 ) THEN
            DO K = 0, NZ+1
               DO J = 0, NY+1
                  COUNT_SEND_WEST = COUNT_SEND_WEST + 1
                 ! SEND_BUFFER_WEST ( COUNT_SEND_WEST ) = GRID ( NX, J, K )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NZ+1, 2)
         END IF

      ELSE IF ( DIR == DIR_NORTH_SOUTH ) THEN
      
         ! North boundary
      
         IF ( NEIGHBORS(NORTH) /= -1 ) THEN
            DO K = 0, NZ+1
               DO I = 0, NX+1
                  COUNT_SEND_NORTH = COUNT_SEND_NORTH + 1
                  !SEND_BUFFER_NORTH ( COUNT_SEND_NORTH ) = GRID ( I, 1, K )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)
         END IF
      
         ! South boundary
      
         IF ( NEIGHBORS(SOUTH) /= -1 ) THEN
            DO K = 0, NZ+1
               DO I = 0, NX+1
                  COUNT_SEND_SOUTH = COUNT_SEND_SOUTH + 1
                 ! SEND_BUFFER_SOUTH ( COUNT_SEND_SOUTH ) = GRID ( I, NY, K )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)
         END IF
      
      END IF 

      MG_PERF(mype+1)%TIME_PACK_PE = MG_PERF(mype+1)%TIME_PACK_PE + MPI_WTIME ( ) - TIME_START
   
   END SUBROUTINE MG_PACK_1DIR
      
END MODULE MG_PACK_1DIR_MOD
