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

MODULE MG_SVCP_NODIAGS_INIT_MOD

   USE MG_UTILS_MOD
   USE MG_GET_FACE_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! This routine loads the ghost space so that the initial computation is correct.
   ! This no computation takes place and no profiling information is collected.
   ! =====================================================================================

CONTAINS
   
   SUBROUTINE MG_SVCP_NODIAGS_INIT ( IVAR, IERR, mype )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         IVAR

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
     !    GRID

         
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
         NUM_RECVS_OUTSTANDING,    &
         OFFSET

      INTEGER ::      &
         ISTAT(MPI_STATUS_SIZE),   &
         ISTAT_ALL(MAX_NUM_NEIGHBORS, MPI_STATUS_SIZE)

      REAL(KIND=MG_REAL) ::   &
         SLICE_BACK,          &   ! Temporary
         SLICE_MINE,          &   ! Temporary
         SLICE_FRONT              ! Temporary

      REAL(KIND=MG_REAL), PARAMETER :: &
         TWENTYSEVENTH = 1.0 / 27.0
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      ! Post receives.

      BUFFER_START_SOUTH = 0
      BUFFER_START_NORTH = 0

      linfo(mype+1)%NUM_RECVS = 0

      ! BACK and FRONT faces transmitted as faces.

      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN

         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_BACK
         CALL MPI_IRECV (  (NX+2)*(NY+2), MG_MPI_REAL, &
         linfo(mype+1)%NEIGHBORS(BACK),  &
                          linfo(mype+1)%MSG_TAGS(FRONT), linfo(mype+1)%MPI_COMM_MG, &
                          linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_IRECV(BACK)', 1, mype )

         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN

         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_FRONT
         CALL MPI_IRECV (  (NX+2)*(NY+2), MG_MPI_REAL, &
         linfo(mype+1)%NEIGHBORS(FRONT),  &
                          linfo(mype+1)%MSG_TAGS(BACK), linfo(mype+1)%MPI_COMM_MG, &
                           linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), IERR )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_IRECV(FRONT)', 1, mype )

         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + 1

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN

         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_EAST
         OFFSET = 1
         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NY, MG_MPI_REAL, &
            	 linfo(mype+1)%NEIGHBORS(EAST),  &
                             linfo(mype+1)%MSG_TAGS(WEST)+K, linfo(mype+1)%MPI_COMM_MG, &
                              linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), &
                             IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_IRECV(EAST)', K, mype )
            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1
            OFFSET = OFFSET + NY

         END DO

         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN

         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_WEST
         OFFSET = 1
         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NY, MG_MPI_REAL, linfo(mype+1)%NEIGHBORS(WEST),  &
                             linfo(mype+1)%MSG_TAGS(EAST)+K, linfo(mype+1)%MPI_COMM_MG, linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), &
                             IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_IRECV(WEST)', K, mype )
            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1
            OFFSET = OFFSET + NY

         END DO
  
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN

         BUFFER_START_NORTH = 1
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_NORTH

         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NX, MG_MPI_REAL, &
            	linfo(mype+1)%NEIGHBORS(NORTH),  &
                             linfo(mype+1)%MSG_TAGS(SOUTH)+K, linfo(mype+1)%MPI_COMM_MG, &
                             linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET), &
                             IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_IRECV(NORTH)', K, mype )

            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1
            BUFFER_START_NORTH = BUFFER_START_NORTH + NX

         END DO
     
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

      END IF
   
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN

         BUFFER_START_SOUTH = 1
         MSG_REQS_OFFSET = linfo(mype+1)%REQ_OFFSET_RECV_SOUTH

         DO K = 2, NZ - 1

            CALL MPI_IRECV (  NX, MG_MPI_REAL,  &
            linfo(mype+1)%NEIGHBORS(SOUTH),  &
                             linfo(mype+1)%MSG_TAGS(NORTH)+K, linfo(mype+1)%MPI_COMM_MG, &
                             linfo(mype+1)%MSG_REQS(MSG_REQS_OFFSET),  &
                             IERR )
            CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_IRECV(SOUTH)', K, mype )

            BUFFER_START_SOUTH = BUFFER_START_SOUTH + NX
            MSG_REQS_OFFSET = MSG_REQS_OFFSET + 1

         END DO
  
         linfo(mype+1)%NUM_RECVS = linfo(mype+1)%NUM_RECVS + NZ

      END IF

      ! End posting of receives.

      ! Copy initial data into WORK for transmission. (No computation here.)

      DO K = 0, NZ+1
         DO J = 0, NY+1
            DO I = 0, NX+1
              ! winfo(mype+1)%WORK(I,J,K) = GRID(I,J,K)
            END DO
         END DO

         call SSTMAC_compute_loop2(0, NY+1, 0, NX+1, 1)

         IF ( K == 1 ) THEN
            IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN
               CALL MPI_SEND ( (NX+2)*(NY+2), MG_MPI_REAL,  &
                	linfo(mype+1)%NEIGHBORS(BACK),  &
                               linfo(mype+1)%MSG_TAGS(BACK), linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_SEND(BACK)', I, mype )

            END IF
         ELSE IF ( K == NZ ) THEN
            IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN
               CALL MPI_SEND (  (NX+2)*(NY+2), MG_MPI_REAL, &
               		linfo(mype+1)%NEIGHBORS(FRONT),  &
                               linfo(mype+1)%MSG_TAGS(FRONT), linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_SEND(FRONT)', I, mype )

            END IF
         ELSE
            IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN
               CALL MPI_SEND (  NY, MG_MPI_REAL, &
               		linfo(mype+1)%NEIGHBORS(EAST),  &
                               linfo(mype+1)%MSG_TAGS(EAST)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_SEND(EAST)', I, mype )

            END IF

            IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN
               CALL MPI_SEND (  NY, MG_MPI_REAL, &
               		linfo(mype+1)%NEIGHBORS(WEST),  &
                               linfo(mype+1)%MSG_TAGS(WEST)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_SEND(WEST)', I, mype )
     
            END IF

            IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
               ! Pack into NX length buffers.
               DO I = 1, NX
                  linfo(mype+1)%SEND_BUFFER ( I ) = winfo(mype+1)%WORK ( I, NY, K )
               END DO
               CALL MPI_SEND (  NX, MG_MPI_REAL, &
               		linfo(mype+1)%NEIGHBORS(NORTH),  &
                               linfo(mype+1)%MSG_TAGS(NORTH)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_SEND(NORTH)', I, mype )
     
            END IF
      
            IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
               ! Pack into NX length buffers.
               DO I = 1, NX
                  linfo(mype+1)%SEND_BUFFER ( I ) = winfo(mype+1)%WORK ( I, 1, K )
               END DO
               linfo(mype+1)%NUM_SENDS = linfo(mype+1)%NUM_SENDS + 1
               CALL MPI_SEND ( NX, MG_MPI_REAL, &
               		linfo(mype+1)%NEIGHBORS(SOUTH),  &
                               linfo(mype+1)%MSG_TAGS(SOUTH)+K, linfo(mype+1)%MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_SEND(SOUTH)', I, mype )
     
            END IF

         END IF

      END DO

      ! Complete receives. Note using BLOCKING SEND, so only receives need completing.

      NUM_RECVS_OUTSTANDING = linfo(mype+1)%NUM_RECVS

      MSG_REQS_LEN = SIZE(linfo(mype+1)%MSG_REQS)
      ! FIXME rbarrett: Can this be a WAITALL? Would that require MSG_REQS all "real", ie not MPI_NULL?
      DO L = 1,linfo(mype+1)%NUM_RECVS
         CALL MPI_WAITANY ( MSG_REQS_LEN, linfo(mype+1)%MSG_REQS, IWHICH, ISTAT, IERR )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_WAITANY', ISTAT(MPI_ERROR), mype )
      
         NUM_RECVS_OUTSTANDING = NUM_RECVS_OUTSTANDING - 1
      
      END DO
      CALL CHECK_ERROR ( NUM_RECVS_OUTSTANDING, 'MG_SVCP_NODIAGS_INIT: NUM_RECVS_OUTSTANDING',  &
                         linfo(mype+1)%NUM_RECVS, mype )

!     CALL MPI_WAITALL ( MSG_REQS_LEN, MSG_REQS, ISTAT_ALL, IERR )
!     CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: MPI_WAITALL', IERR )
      
      ! Now unpack buffers for NORTH and SOUTH faces.

      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
         OFFSET = 0
         CALL MG_GET_FACE ( IVAR, NORTH, OFFSET, IERR, mype )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: Unpacking NORTH face', IVAR, mype )
      END IF
      
      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
         OFFSET = 0
         CALL MG_GET_FACE ( IVAR, SOUTH, OFFSET, IERR, mype )
         CALL CHECK_ERROR ( IERR, 'MG_SVCP_NODIAGS_INIT: Unpacking SOUTH face', IVAR, mype )
      END IF

      linfo(mype+1)%NUM_RECVS = NUM_RECVS_OUTSTANDING

   END SUBROUTINE MG_SVCP_NODIAGS_INIT
   
END MODULE MG_SVCP_NODIAGS_INIT_MOD
