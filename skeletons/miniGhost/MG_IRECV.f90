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

MODULE MG_IRECV_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_IRECV ( IERR, mype )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

         
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------

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

      ! --------------
      ! Post receives:
      ! --------------

      linfo(mype+1)%NUM_RECVS = 0

      linfo(mype+1)%COUNT_RECV_NORTH = linfo(mype+1)%RECV_BUFFER_NORTH_SIZE
      linfo(mype+1)%COUNT_RECV_SOUTH = linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE
      linfo(mype+1)%COUNT_RECV_EAST  = linfo(mype+1)%RECV_BUFFER_EAST_SIZE
      linfo(mype+1)%COUNT_RECV_WEST  = linfo(mype+1)%RECV_BUFFER_WEST_SIZE
      linfo(mype+1)%COUNT_RECV_BACK  = linfo(mype+1)%RECV_BUFFER_BACK_SIZE
      linfo(mype+1)%COUNT_RECV_FRONT = linfo(mype+1)%RECV_BUFFER_FRONT_SIZE

      ! Back boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1
         CALL MPI_IRECV ( linfo(mype+1)%COUNT_RECV_BACK, MG_MPI_REAL, &
                          linfo(mype+1)%NEIGHBORS(BACK), linfo(mype+1)%MSG_TAGS(FRONT),              &
                          linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(BACK), IERR )
         MG_PERF(mype+1)%TIME_RECV_Z_PE = MG_PERF(mype+1)%TIME_RECV_Z_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + linfo(mype+1)%COUNT_RECV_BACK
            IF ( linfo(mype+1)%COUNT_RECV_BACK > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = linfo(mype+1)%COUNT_RECV_BACK
            END IF
            IF ( linfo(mype+1)%COUNT_RECV_BACK < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = linfo(mype+1)%COUNT_RECV_BACK
            END IF
         END IF

      END IF
   
      ! Front boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1
         CALL MPI_IRECV (  linfo(mype+1)%COUNT_RECV_FRONT, MG_MPI_REAL,   &
                          linfo(mype+1)%NEIGHBORS(FRONT), linfo(mype+1)%MSG_TAGS(BACK),                &
                          linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(FRONT), IERR )
         MG_PERF(mype+1)%TIME_RECV_Z_PE = MG_PERF(mype+1)%TIME_RECV_Z_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + linfo(mype+1)%COUNT_RECV_FRONT
            IF ( linfo(mype+1)%COUNT_RECV_FRONT > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = linfo(mype+1)%COUNT_RECV_FRONT
            END IF
            IF ( linfo(mype+1)%COUNT_RECV_FRONT < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = linfo(mype+1)%COUNT_RECV_FRONT
            END IF
         END IF

      END IF
   
      ! East boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1
         CALL MPI_IRECV (  linfo(mype+1)%COUNT_RECV_EAST, MG_MPI_REAL, &
                          linfo(mype+1)%NEIGHBORS(EAST), linfo(mype+1)%MSG_TAGS(WEST),              &
                          linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(EAST), IERR )
         MG_PERF(mype+1)%TIME_RECV_X_PE = MG_PERF(mype+1)%TIME_RECV_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + linfo(mype+1)%COUNT_RECV_EAST
            IF ( linfo(mype+1)%COUNT_RECV_EAST > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = linfo(mype+1)%COUNT_RECV_EAST
            END IF
            IF ( linfo(mype+1)%COUNT_RECV_EAST < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = linfo(mype+1)%COUNT_RECV_EAST
            END IF
         END IF

      END IF
   
      ! West boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1
         CALL MPI_IRECV (  linfo(mype+1)%COUNT_RECV_WEST, MG_MPI_REAL, &
                          linfo(mype+1)%NEIGHBORS(WEST), linfo(mype+1)%MSG_TAGS(EAST),                &
                          linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(WEST), IERR )
         MG_PERF(mype+1)%TIME_RECV_X_PE = MG_PERF(mype+1)%TIME_RECV_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + linfo(mype+1)%COUNT_RECV_WEST
            IF ( linfo(mype+1)%COUNT_RECV_WEST > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = linfo(mype+1)%COUNT_RECV_WEST
            END IF
            IF ( linfo(mype+1)%COUNT_RECV_WEST < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = linfo(mype+1)%COUNT_RECV_WEST
            END IF
         END IF

      END IF
   
      ! North boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1
         CALL MPI_IRECV (  linfo(mype+1)%COUNT_RECV_NORTH, MG_MPI_REAL,   &
                          linfo(mype+1)%NEIGHBORS(NORTH), linfo(mype+1)%MSG_TAGS(SOUTH),                &
                          linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(NORTH), IERR )
         MG_PERF(mype+1)%TIME_RECV_Y_PE = MG_PERF(mype+1)%TIME_RECV_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + linfo(mype+1)%COUNT_RECV_NORTH
            IF ( linfo(mype+1)%COUNT_RECV_NORTH > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = linfo(mype+1)%COUNT_RECV_NORTH
            END IF
            IF ( linfo(mype+1)%COUNT_RECV_NORTH < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = linfo(mype+1)%COUNT_RECV_NORTH
            END IF
         END IF

      END IF
   
      ! South boundary
   
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1
         CALL MPI_IRECV ( linfo(mype+1)%COUNT_RECV_SOUTH,  MG_MPI_REAL, &
                          linfo(mype+1)%NEIGHBORS(SOUTH), linfo(mype+1)%MSG_TAGS(NORTH),                &
                          linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(SOUTH), IERR )
         MG_PERF(mype+1)%TIME_RECV_Y_PE = MG_PERF(mype+1)%TIME_RECV_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + linfo(mype+1)%COUNT_RECV_SOUTH
            IF ( linfo(mype+1)%COUNT_RECV_SOUTH > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = linfo(mype+1)%COUNT_RECV_SOUTH
            END IF
            IF ( linfo(mype+1)%COUNT_RECV_SOUTH < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = linfo(mype+1)%COUNT_RECV_SOUTH
            END IF
         END IF

      END IF

      MG_PERF(mype+1)%TIME_RECV_PE = MG_PERF(mype+1)%TIME_RECV_PE + MPI_WTIME ( ) - TIME_START

      IF ( PROFILE_MG ) THEN
         MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + linfo(mype+1)%NUM_RECVS
      END IF

   END SUBROUTINE MG_IRECV
   
END MODULE MG_IRECV_MOD
