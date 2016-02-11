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

MODULE MG_BSEND_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_BSEND ( IERR, mype )
   
      ! -------------------------------------------------------
      ! Pack all variables for single message to each neighbor.
      ! -------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

          
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER :: &
         I, J                        ! Counters
   
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

      ! ------------------------------------------------------
      ! Construct message buffers across variables, then send.
      ! ------------------------------------------------------
   
      linfo(mype+1)%NUM_SENDS = 0

      ! Back boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
         CALL MPI_SEND (  linfo(mype+1)%COUNT_SEND_BACK, MG_MPI_REAL, &
                         linfo(mype+1)%NEIGHBORS(BACK), linfo(mype+1)%MSG_TAGS(BACK),                &
                         linfo(mype+1)%MPI_COMM_MG, IERR )
         MG_PERF(mype+1)%TIME_SEND_Z_PE = MG_PERF(mype+1)%TIME_SEND_Z_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + linfo(mype+1)%COUNT_SEND_BACK
            IF ( linfo(mype+1)%COUNT_SEND_BACK > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MAX = linfo(mype+1)%COUNT_SEND_BACK
            END IF
            IF ( linfo(mype+1)%COUNT_SEND_BACK < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MIN = linfo(mype+1)%COUNT_SEND_BACK
            END IF
         END IF

      END IF
   
      ! Front boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
         CALL MPI_SEND (  linfo(mype+1)%COUNT_SEND_FRONT, MG_MPI_REAL, &
                         linfo(mype+1)%NEIGHBORS(FRONT), linfo(mype+1)%MSG_TAGS(FRONT),                &
                         linfo(mype+1)%MPI_COMM_MG, IERR )
         MG_PERF(mype+1)%TIME_SEND_Z_PE = MG_PERF(mype+1)%TIME_SEND_Z_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + linfo(mype+1)%COUNT_SEND_FRONT
            IF ( linfo(mype+1)%COUNT_SEND_FRONT > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MAX = linfo(mype+1)%COUNT_SEND_FRONT
            END IF
            IF ( linfo(mype+1)%COUNT_SEND_FRONT < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MIN = linfo(mype+1)%COUNT_SEND_FRONT
            END IF
         END IF

      END IF
   
      ! East boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
         CALL MPI_SEND (  linfo(mype+1)%COUNT_SEND_EAST, MG_MPI_REAL, &
                         linfo(mype+1)%NEIGHBORS(EAST), linfo(mype+1)%MSG_TAGS(EAST),                &
                         linfo(mype+1)%MPI_COMM_MG, IERR )
         MG_PERF(mype+1)%TIME_SEND_X_PE = MG_PERF(mype+1)%TIME_SEND_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + linfo(mype+1)%COUNT_SEND_EAST
            IF ( linfo(mype+1)%COUNT_SEND_EAST > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MAX = linfo(mype+1)%COUNT_SEND_EAST
            END IF
            IF ( linfo(mype+1)%COUNT_SEND_EAST < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MIN = linfo(mype+1)%COUNT_SEND_EAST
            END IF
         END IF

      END IF
   
      ! West boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
         CALL MPI_SEND (  linfo(mype+1)%COUNT_SEND_WEST, MG_MPI_REAL, &
                         linfo(mype+1)%NEIGHBORS(WEST), linfo(mype+1)%MSG_TAGS(WEST),                &
                         linfo(mype+1)%MPI_COMM_MG, IERR )
         MG_PERF(mype+1)%TIME_SEND_X_PE = MG_PERF(mype+1)%TIME_SEND_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + linfo(mype+1)%COUNT_SEND_WEST
            IF ( linfo(mype+1)%COUNT_SEND_WEST > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MAX = linfo(mype+1)%COUNT_SEND_WEST
            END IF
            IF ( linfo(mype+1)%COUNT_SEND_WEST < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MIN = linfo(mype+1)%COUNT_SEND_WEST
            END IF
         END IF

      END IF
   
      ! North boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
         CALL MPI_SEND (  linfo(mype+1)%COUNT_SEND_NORTH, MG_MPI_REAL, &
                         linfo(mype+1)%NEIGHBORS(NORTH), linfo(mype+1)%MSG_TAGS(NORTH),                &
                         linfo(mype+1)%MPI_COMM_MG, IERR )
         MG_PERF(mype+1)%TIME_SEND_Y_PE = MG_PERF(mype+1)%TIME_SEND_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + linfo(mype+1)%COUNT_SEND_NORTH
            IF ( linfo(mype+1)%COUNT_SEND_NORTH > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MAX = linfo(mype+1)%COUNT_SEND_NORTH
            END IF
            IF ( linfo(mype+1)%COUNT_SEND_NORTH < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MIN = linfo(mype+1)%COUNT_SEND_NORTH
            END IF
         END IF

      END IF
   
      ! South boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
         CALL MPI_SEND (  linfo(mype+1)%COUNT_SEND_SOUTH, MG_MPI_REAL, &
                         linfo(mype+1)%NEIGHBORS(SOUTH), linfo(mype+1)%MSG_TAGS(SOUTH),                &
                         linfo(mype+1)%MPI_COMM_MG, IERR )
         MG_PERF(mype+1)%TIME_SEND_Y_PE = MG_PERF(mype+1)%TIME_SEND_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + linfo(mype+1)%COUNT_SEND_SOUTH
            IF ( linfo(mype+1)%COUNT_SEND_SOUTH > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MAX = linfo(mype+1)%COUNT_SEND_SOUTH
            END IF
            IF ( linfo(mype+1)%COUNT_SEND_SOUTH < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
               MG_PERF(mype+1)%SEND_COUNT_MIN = linfo(mype+1)%COUNT_SEND_SOUTH
            END IF
         END IF

      END IF

      linfo(mype+1)%COUNT_SEND_NORTH = 0
      linfo(mype+1)%COUNT_SEND_SOUTH = 0
      linfo(mype+1)%COUNT_SEND_EAST  = 0
      linfo(mype+1)%COUNT_SEND_WEST  = 0
      linfo(mype+1)%COUNT_SEND_BACK  = 0
      linfo(mype+1)%COUNT_SEND_FRONT = 0

      MG_PERF(mype+1)%TIME_SEND_PE = MG_PERF(mype+1)%TIME_SEND_PE + MPI_WTIME ( ) - TIME_START

      IF ( PROFILE_MG ) THEN
         MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + linfo(mype+1)%NUM_SENDS
      END IF

   END SUBROUTINE MG_BSEND
   
END MODULE MG_BSEND_MOD
