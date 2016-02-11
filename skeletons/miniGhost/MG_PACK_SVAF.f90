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

MODULE MG_PACK_SVAF_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_PACK_SVAF ( IERR, mype )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! Note that ghost space is sent as well (i.e. rows 0 and NX+1), 
      ! necessary for stencils with diagonals and irrelevant for stencils
      ! without diagonals. Could be a separate routine, but no matter.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

    !  REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(IN) :: &
      !   GRID

 		
         integer :: mype
      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER ::        &
         I, J, K                         ! Counters

      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &
         TIME_START_DIR

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if !defined ( _MG_DEBUG )
      IF ( NUMPES == 1 ) &
         RETURN
#endif

      TIME_START = MPI_WTIME ( )

      ! Back boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN

         ! This face is contiguous in memory, so sent directly from MG_SEND_SVAF.
         IERR = 0

      END IF
   
      ! Front boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN

         ! This face is contiguous in memory, so sent directly from MG_SEND_SVAF.
         IERR = 0

      END IF
   
      ! East boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
          DO K = 0, NZ+1
            DO J = 0, NY+1
               linfo(mype+1)%COUNT_SEND_EAST = linfo(mype+1)%COUNT_SEND_EAST + 1
              ! linfo(mype+1)%SEND_BUFFER_EAST ( linfo(mype+1)%COUNT_SEND_EAST ) = GRID ( 1, J, K )
            END DO
         END DO
         MG_PERF(mype+1)%TIME_PACK_X_PE = MG_PERF(mype+1)%TIME_PACK_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR
         call SSTMAC_compute_loop2(0, NZ+1, 0, NY+1, 2)
      END IF
   
      ! West boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         DO K = 0, NZ+1
            DO J = 0, NY+1
               linfo(mype+1)%COUNT_SEND_WEST = linfo(mype+1)%COUNT_SEND_WEST + 1
              ! linfo(mype+1)%SEND_BUFFER_WEST ( linfo(mype+1)%COUNT_SEND_WEST ) = GRID ( NX, J, K )
            END DO
         END DO
         MG_PERF(mype+1)%TIME_PACK_X_PE = MG_PERF(mype+1)%TIME_PACK_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR
 		 call SSTMAC_compute_loop2(0, NZ+1, 0, NY+1, 2)
      END IF
   
      ! North boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         DO K = 0, NZ+1
            DO I = 0, NX+1
               linfo(mype+1)%COUNT_SEND_NORTH = linfo(mype+1)%COUNT_SEND_NORTH + 1
              ! linfo(mype+1)%SEND_BUFFER_NORTH ( linfo(mype+1)%COUNT_SEND_NORTH ) = GRID ( I, 1, K )
            END DO
         END DO
         MG_PERF(mype+1)%TIME_PACK_Y_PE = MG_PERF(mype+1)%TIME_PACK_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR
      	call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)
      END IF
   
      ! South boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         DO K = 0, NZ+1
            DO I = 0, NX+1
               linfo(mype+1)%COUNT_SEND_SOUTH = linfo(mype+1)%COUNT_SEND_SOUTH + 1
              ! linfo(mype+1)%SEND_BUFFER_SOUTH ( linfo(mype+1)%COUNT_SEND_SOUTH ) = GRID ( I, NY, K )
            END DO
         END DO
         MG_PERF(mype+1)%TIME_PACK_Y_PE = MG_PERF(mype+1)%TIME_PACK_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR
      	call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)
      END IF
   
      MG_PERF(mype+1)%TIME_PACK_PE = MG_PERF(mype+1)%TIME_PACK_PE + MPI_WTIME ( ) - TIME_START

   END SUBROUTINE MG_PACK_SVAF
   
END MODULE MG_PACK_SVAF_MOD
