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

MODULE MG_SVCP_3D27PT_MOD

   USE MG_UTILS_MOD
   USE MG_GET_FACE_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Stencil for a single variable, with inter-process communication interleaved as seems
   ! natural to the computation. The computation is across X-Y faces, sweeping across Z.
   ! So the communication configuration for this version is:
   !
   ! 1) FRONT and BACK neighbors: X-Y faces, being contiguous in memory, are transmitted 
   !    as a single message. In order to avoid intermediate buffering, ghost data is
   !    included in the message.
   ! 2) EAST and WEST neighbors: Each column of the Y-Z face is contiguous in memory, 
   !    and therefore transmitted as soon as available. That is, NZ messages of length NY 
   !    are transmitted. 
   ! 3) NORTH and SOUTH neighbors: No cell of the X-Z face is contiguous in memory. 
   !    For this version, we aggregate rows in the X direction, sending NZ messages
   !    of length NX. A single buffer is used, although this is only relevant on the receive
   !    side as individual messages are transmitted for each NX accumulation.
   ! 4) Non-blocking recvs are posted, followed computation with BLOCKING sends interleaved,
   !    followed by completion of receives, then unpacking of the NORTH and SOUTH receive
   !    buffers into the appropriate area of the array.
   ! =====================================================================================

CONTAINS
   
   SUBROUTINE MG_SVCP_3D27PT (  GRID_OUT, IVAR, IERR, mype )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         IVAR

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(IN) :: &
     !    GRID_IN

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(OUT) :: &
         GRID_OUT

         
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER ::      &
         BUFFER_START_NORTH,       &
         BUFFER_START_SOUTH,       &
         COUNTER,                  &
         I, II, J, JJ, K, L,       &  ! Counters
         IWHICH,                   &
         MSG_REQS_LEN,             &
         MSG_REQS_OFFSET,          &
         NUM_GRID_PTS,             &
         NUM_RECVS_OUTSTANDING,    &
         OFFSET

      INTEGER ::      &
         ISTAT(MPI_STATUS_SIZE),   &
         ISTAT_ALL(MAX_NUM_NEIGHBORS, MPI_STATUS_SIZE)

      REAL(KIND=MG_REAL) ::   &
         SLICE_BACK,          &   ! Temporary
         SLICE_MINE,          &   ! Temporary
         SLICE_FRONT              ! Temporary

      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &
         TIME_START_DIR,          &
         TIME_PACK

      REAL(KIND=MG_REAL), PARAMETER :: &
         TWENTYSEVENTH = 1.0 / 27.0
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      ! Post receives.

      TIME_START = MPI_WTIME ( )

      BUFFER_START_SOUTH = 0
      BUFFER_START_NORTH = 0

      linfo(mype+1)%NUM_RECVS = 0

      ! BACK and FRONT faces transmitted as faces.

      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_BACK
         CALL MPI_IRECV (  (NX+2)*(NY+2), MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(BACK),  &
                          linfo(mype+1)%MSG_TAGS(FRONT), linfo(mype+1)%MPI_COMM_MG, &
                          linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_IRECV(GRID_OUT(BACK))', 1, mype )
         MG_PERF(mype+1)%TIME_RECV_Z_PE = MG_PERF(mype+1)%TIME_RECV_Z_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + 1
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + ( NX*NY )
            IF ( NX*NY > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = NX*NY
            END IF
            IF ( NX*NY < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = NX*NY
            END IF
         END IF

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_FRONT
         CALL MPI_IRECV (  (NX+2)*(NY+2), MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(FRONT),  &
                          linfo(mype+1)%MSG_TAGS(BACK), linfo(mype+1)%MPI_COMM_MG, &
                          linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_IRECV(GRID_OUT(FRONT))', 1, mype )
         MG_PERF(mype+1)%TIME_RECV_Z_PE = MG_PERF(mype+1)%TIME_RECV_Z_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + 1
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + ( NX*NY )
            IF ( NX*NY > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = NX*NY
            END IF
            IF ( NX*NY < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = NX*NY
            END IF
         END IF
      END IF

      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_EAST
         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NY+2, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(EAST),  &
                             linfo(mype+1)%MSG_TAGS(WEST)+K, linfo(mype+1)%MPI_COMM_MG, &
                             linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_IRECV(GRID_OUT(EAST))', K, mype )
            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1

         END DO
         MG_PERF(mype+1)%TIME_RECV_X_PE = MG_PERF(mype+1)%TIME_RECV_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR

         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + NZ
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + ( NY*NZ )
            IF ( ( NY*NZ ) > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = ( NY*NZ )
            END IF
            IF ( ( NY*NZ ) < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = ( NY*NZ )
            END IF
         END IF
      END IF

      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_WEST
         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NY+2, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(WEST),  &
                             linfo(mype+1)%MSG_TAGS(EAST)+K, linfo(mype+1)%MPI_COMM_MG, &
                             linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_IRECV(GRID_OUT(WEST))', K, mype )
            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1

         END DO
         MG_PERF(mype+1)%TIME_RECV_X_PE = MG_PERF(mype+1)%TIME_RECV_X_PE + MPI_WTIME() - &
                                  TIME_START_DIR
  
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + NZ
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + ( NY*NZ )
            IF ( ( NY*NZ ) > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = ( NY*NZ )
            END IF
            IF ( ( NY*NZ ) < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = ( NY*NZ )
            END IF
         END IF

      END IF

      ! NORTH and SOUTH are buffered, since data point-wise non-contiguous, with stride (NX+2)*(NY+2).

      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         BUFFER_START_NORTH = 1
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_NORTH

         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NX, MG_MPI_REAL, &
             linfo(mype+1)%NEIGHBORS(NORTH),  &
                             linfo(mype+1)%MSG_TAGS(SOUTH)+K, linfo(mype+1)%MPI_COMM_MG, &
                             linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_IRECV(NORTH)', K, mype )

            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1
            BUFFER_START_NORTH = BUFFER_START_NORTH + NX

         END DO
         MG_PERF(mype+1)%TIME_RECV_Y_PE = MG_PERF(mype+1)%TIME_RECV_Y_PE + MPI_WTIME() - &
                                  TIME_START_DIR
     
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + NZ
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + ( NX*NZ )
            IF ( ( NX*NZ ) > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = ( NX*NZ )
            END IF
            IF ( ( NX*NZ ) < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = ( NX*NZ )
            END IF
         END IF

      END IF
   
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN

         TIME_START_DIR = MPI_WTIME()
         BUFFER_START_SOUTH = 1
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_SOUTH

         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NX, MG_MPI_REAL, &
             linfo(mype+1)%NEIGHBORS(SOUTH),  &
                             linfo(mype+1)%MSG_TAGS(NORTH)+K, linfo(mype+1)%MPI_COMM_MG, &
                             linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_IRECV(SOUTH)', K, mype )

            BUFFER_START_SOUTH = BUFFER_START_SOUTH + NX
            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1

         END DO
         TIME_START_DIR = MPI_WTIME()
  
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

         IF ( PROFILE_MG ) THEN
            MG_PERF(mype+1)%NUM_RECVS = MG_PERF(mype+1)%NUM_RECVS + NZ
            MG_PERF(mype+1)%RECV_COUNT  = MG_PERF(mype+1)%RECV_COUNT + ( NX*NZ )
            IF ( ( NX*NZ ) > MG_PERF(mype+1)%RECV_COUNT_MAX ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MAX = ( NX*NZ )
            END IF
            IF ( ( NX*NZ ) < MG_PERF(mype+1)%RECV_COUNT_MIN ) THEN
               MG_PERF(mype+1)%RECV_COUNT_MIN = ( NX*NZ )
            END IF
         END IF

      END IF

      MG_PERF(mype+1)%TIME_RECV_PE = MG_PERF(mype+1)%TIME_RECV_PE + MPI_WTIME ( ) - TIME_START

      ! End posting of receives.

      DO K = 1, NZ 

         TIME_START = MPI_WTIME ( )
 
         DO J = 1, NY
            DO I = 1, NX

              ! SLICE_BACK =  GRID_IN(I-1,J-1,K-1) + GRID_IN(I-1,J,K-1) + GRID_IN(I-1,J+1,K-1) + &
              !               GRID_IN(I  ,J-1,K-1) + GRID_IN(I  ,J,K-1) + GRID_IN(I  ,J+1,K-1) + &
              !               GRID_IN(I+1,J-1,K-1) + GRID_IN(I+1,J,K-1) + GRID_IN(I+1,J+1,K-1)

              ! SLICE_MINE =  GRID_IN(I-1,J-1,K)   + GRID_IN(I-1,J,K)   + GRID_IN(I-1,J+1,K) + &
              !               GRID_IN(I  ,J-1,K)   + GRID_IN(I  ,J,K)   + GRID_IN(I  ,J+1,K) + &
              !               GRID_IN(I+1,J-1,K)   + GRID_IN(I+1,J,K)   + GRID_IN(I+1,J+1,K)

              ! SLICE_FRONT = GRID_IN(I-1,J-1,K+1) + GRID_IN(I-1,J,K+1) + GRID_IN(I-1,J+1,K+1) + &
              !               GRID_IN(I  ,J-1,K+1) + GRID_IN(I  ,J,K+1) + GRID_IN(I  ,J+1,K+1) + &
              !               GRID_IN(I+1,J-1,K+1) + GRID_IN(I+1,J,K+1) + GRID_IN(I+1,J+1,K+1)

               GRID_OUT(I,J,K) = ( SLICE_BACK + SLICE_MINE + SLICE_FRONT ) * TWENTYSEVENTH

            END DO
         END DO

         call SSTMAC_compute_loop2(1, NY, 1, NX, 25)

         MG_PERF(mype+1)%TIME_STENCIL_PE = MG_PERF(mype+1)%TIME_STENCIL_PE + MPI_WTIME ( ) - TIME_START

         TIME_START = MPI_WTIME ( )

         TIME_PACK = 0.0

         IF ( K == 1 ) THEN
            IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN
               TIME_START_DIR = MPI_WTIME()
               CALL MPI_SEND ( (NX+2)*(NY+2), MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(BACK),  &
                               linfo(mype+1)%MSG_TAGS(BACK), linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_SEND(BACK)', I, mype )
               MG_PERF(mype+1)%TIME_SEND_Z_PE = MG_PERF(mype+1)%TIME_SEND_Z_PE + MPI_WTIME() - &
                                        TIME_START_DIR

               IF ( PROFILE_MG ) THEN 
                  MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + 1
                  MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + NX*NY
                  IF ( NX*NY > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MAX = NX*NY
                  END IF
                  IF ( NX*NY < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MIN = NX*NY
                  END IF
               END IF
            END IF
         ELSE IF ( K == NZ ) THEN
            IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN
               TIME_START_DIR = MPI_WTIME()
               CALL MPI_SEND ( (NX+2)*(NY+2), MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(FRONT),  &
                               linfo(mype+1)%MSG_TAGS(FRONT), linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_SEND(FRONT)', I, mype )
               MG_PERF(mype+1)%TIME_SEND_Z_PE = MG_PERF(mype+1)%TIME_SEND_Z_PE + MPI_WTIME() - &
                                        TIME_START_DIR

               IF ( PROFILE_MG ) THEN
                  MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + 1
                  MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + NX*NY
                  IF ( NX*NY > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MAX = NX*NY
                  END IF
                  IF ( NX*NY < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MIN = NX*NY
                  END IF
               END IF
            END IF
         ELSE
            IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN
               TIME_START_DIR = MPI_WTIME()
               CALL MPI_SEND ( NY, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(EAST),  &
                               linfo(mype+1)%MSG_TAGS(EAST)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_SEND(EAST)', I, mype )
               MG_PERF(mype+1)%TIME_SEND_X_PE = MG_PERF(mype+1)%TIME_SEND_X_PE + MPI_WTIME() - &
                                        TIME_START_DIR

               IF ( PROFILE_MG ) THEN
                  MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + 1
                  MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + NY
                  IF ( NY > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MAX = NY
                  END IF
                  IF ( NY < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MIN = NY
                  END IF
               END IF
            END IF

            IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN
               TIME_START_DIR = MPI_WTIME()
               CALL MPI_SEND ( NY, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(WEST),  &
                               linfo(mype+1)%MSG_TAGS(WEST)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_SEND(WEST)', I, mype )
               MG_PERF(mype+1)%TIME_SEND_X_PE = MG_PERF(mype+1)%TIME_SEND_X_PE + MPI_WTIME() - &
                                        TIME_START_DIR
     
               IF ( PROFILE_MG ) THEN
                  MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + 1
                  MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + NY
                  IF ( NY > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MAX = NY
                  END IF
                  IF ( NY < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MIN = NY
                  END IF
               END IF
            END IF

            IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
               ! Pack into NX length buffers.
               TIME_START_DIR = MPI_WTIME()
               DO I = 1, NX
                  linfo(mype+1)%SEND_BUFFER ( I ) = GRID_OUT ( I, NY, K )
               END DO
               TIME_PACK = TIME_PACK + MPI_WTIME() - TIME_START_DIR
               TIME_START_DIR = MPI_WTIME()
               CALL MPI_SEND ( NX, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(NORTH),  &
                               linfo(mype+1)%MSG_TAGS(NORTH)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_SEND(NORTH)', I, mype )
               MG_PERF(mype+1)%TIME_SEND_Y_PE = MG_PERF(mype+1)%TIME_SEND_Y_PE + MPI_WTIME() - &
                                        TIME_START_DIR
     
               IF ( PROFILE_MG ) THEN
                  MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + 1
                  MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + NX
                  IF ( NX > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MAX = NX
                  END IF
                  IF ( NX < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MIN = NX
                  END IF
               END IF
            END IF
      
            IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
               ! Pack into NX length buffers.
               TIME_START_DIR = MPI_WTIME()
               DO I = 1, NX
                  linfo(mype+1)%SEND_BUFFER ( I ) = GRID_OUT ( I, 1, K )
               END DO
               TIME_PACK = TIME_PACK + MPI_WTIME() - TIME_START_DIR
               TIME_START_DIR = MPI_WTIME()
               CALL MPI_SEND ( NX, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(SOUTH),  &
                               linfo(mype+1)%MSG_TAGS(SOUTH)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_SEND(SOUTH)', I, mype )
               MG_PERF(mype+1)%TIME_SEND_Y_PE = MG_PERF(mype+1)%TIME_SEND_Y_PE + MPI_WTIME() - &
                                        TIME_START_DIR
     
               IF ( PROFILE_MG ) THEN 
                  MG_PERF(mype+1)%NUM_SENDS = MG_PERF(mype+1)%NUM_SENDS + 1
                  MG_PERF(mype+1)%SEND_COUNT  = MG_PERF(mype+1)%SEND_COUNT + NX
                  IF ( NX > MG_PERF(mype+1)%SEND_COUNT_MAX ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MAX = NX
                  END IF
                  IF ( NX < MG_PERF(mype+1)%SEND_COUNT_MIN ) THEN
                     MG_PERF(mype+1)%SEND_COUNT_MIN = NX
                  END IF
               END IF
            END IF

         END IF

         MG_PERF(mype+1)%TIME_SEND_PE = MG_PERF(mype+1)%TIME_SEND_PE + MPI_WTIME ( ) - &
                                TIME_START - TIME_PACK
         MG_PERF(mype+1)%TIME_PACK_PE = MG_PERF(mype+1)%TIME_PACK_PE + TIME_PACK
         MG_PERF(mype+1)%TIME_PACK_Y_PE = MG_PERF(mype+1)%TIME_PACK_Y_PE + TIME_PACK

      END DO

      call SSTMAC_compute_loop(1, NZ, 9)

      ! Complete receives. Note using BLOCKING SEND, so only receives need completing.

      TIME_START = MPI_WTIME ( )

      NUM_RECVS_OUTSTANDING = linfo(mype+1)%NUM_RECVS

      MSG_REQS_LEN = SIZE(linfo(mype+1)%MSG_REQS)
      ! FIXME rbarrett: Can this be a WAITALL? Would that require MSG_REQS all "real", ie not MPI_NULL?
      DO L = 1, linfo(mype+1)%NUM_RECVS
         TIME_START_DIR = MPI_WTIME()
         CALL MPI_WAITANY ( MSG_REQS_LEN, linfo(mype+1)%MSG_REQS, IWHICH, ISTAT, IERR )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_WAITANY', ISTAT(MPI_ERROR), mype )
         IF (IWHICH.EQ.BACK .OR. IWHICH.EQ.FRONT) THEN
            MG_PERF(mype+1)%TIME_WAIT_Z_PE = MG_PERF(mype+1)%TIME_WAIT_Z_PE + MPI_WTIME() - &
                                     TIME_START_DIR
         ELSE IF (IWHICH.EQ.EAST .OR. IWHICH.EQ.WEST) THEN
            MG_PERF(mype+1)%TIME_WAIT_X_PE = MG_PERF(mype+1)%TIME_WAIT_X_PE + MPI_WTIME() - &
                                     TIME_START_DIR
         ELSE IF (IWHICH.EQ.NORTH .OR. IWHICH.EQ.SOUTH) THEN
            MG_PERF(mype+1)%TIME_WAIT_Y_PE = MG_PERF(mype+1)%TIME_WAIT_Y_PE + MPI_WTIME() - &
                                     TIME_START_DIR
         END IF
      
         NUM_RECVS_OUTSTANDING = NUM_RECVS_OUTSTANDING - 1
      
      END DO
      CALL CHECK_ERROR ( NUM_RECVS_OUTSTANDING, 'MG_SVCP_3D27PT: NUM_RECVS_OUTSTANDING',  &
                         linfo(mype+1)%NUM_RECVS, mype )




      MG_PERF(mype+1)%TIME_WAIT_PE = MG_PERF(mype+1)%TIME_WAIT_PE + MPI_WTIME ( ) - TIME_START

!     CALL MPI_WAITALL ( MSG_REQS_LEN, MSG_REQS, ISTAT_ALL, IERR )
!     CALL CHECK_ERROR ( IERR, 'MG_SVCP_3D27PT: MPI_WAITALL', IERR )
      
      ! Now unpack buffers for NORTH and SOUTH faces.

      TIME_START = MPI_WTIME ( )

      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
         OFFSET = 0
         DO K = 0, NZ+1
            DO I = 0, NX+1
               OFFSET = OFFSET + 1
               GRID_OUT ( I, NY+1, K ) = linfo(mype+1)%RECV_BUFFER_NORTH ( OFFSET )
            END DO
         END DO
         call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)
      END IF
      
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
         OFFSET = 0
         DO K = 0, NZ+1
            DO I = 0, NX+1
               OFFSET = OFFSET + 1
               GRID_OUT ( I, 0, K ) = linfo(mype+1)%RECV_BUFFER_SOUTH ( OFFSET )
            END DO
         END DO
         call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)
      END IF



      MG_PERF(mype+1)%TIME_UNPACK_PE = MG_PERF(mype+1)%TIME_UNPACK_PE + MPI_WTIME ( ) - TIME_START
      MG_PERF(mype+1)%TIME_UNPACK_Y_PE = MG_PERF(mype+1)%TIME_UNPACK_PE

      linfo(mype+1)%NUM_RECVS = 0

      NUM_GRID_PTS = NX*NY*NZ

      MG_PERF(mype+1)%NUM_ADDS  = MG_PERF(mype+1)%NUM_ADDS  + 26*(NUM_GRID_PTS)  
      MG_PERF(mype+1)%NUM_MULTS = MG_PERF(mype+1)%NUM_MULTS + NUM_GRID_PTS
      MG_PERF(mype+1)%NUM_COPY = MG_PERF(mype+1)%NUM_COPY + NUM_GRID_PTS

   END SUBROUTINE MG_SVCP_3D27PT
   
END MODULE MG_SVCP_3D27PT_MOD
