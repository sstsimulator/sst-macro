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

MODULE MG_UNPACK_AGG_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_GET_FACE_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
    SUBROUTINE MG_UNPACK_AGG ( IERR, mype )
   
      ! -------------------------------------------------------
      ! Unpack all variables for single message to each neighbor.
      ! -------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

             
         integer :: mype, ind

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER :: &
         IWHICH,                      &  ! MPI_Wait_any SRC process
         I, L,                        &  ! Counters
         NUM_RECVS_OUTSTANDING,       &  ! Keep track of progress
         NUM_SENDS_OUTSTANDING,       &
         OFFSET
   
      INTEGER ::     &
         ISTAT(MPI_STATUS_SIZE)

      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &
         TIME_START_DIR,          &
         TIME_WAIT,               &
         TIME_WAIT_X,             &
         TIME_WAIT_Y,             &
         TIME_WAIT_Z

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0
      TIME_WAIT_X = 0.0
      TIME_WAIT_Y = 0.0
      TIME_WAIT_Z = 0.0

#if !defined ( _MG_DEBUG )
      IF ( NUMPES == 1 ) &
         RETURN
#endif

      TIME_START = MPI_WTIME ( )

      NUM_SENDS_OUTSTANDING = linfo(mype+1)%NUM_SENDS
      NUM_RECVS_OUTSTANDING = linfo(mype+1)%NUM_RECVS

       !WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): num_sends: ',NUM_SENDS_OUTSTANDING,', num_recvs: ',NUM_RECVS_OUTSTANDING

! DO L = 1, linfo(mype+1)%MAX_NUM_RECVS + linfo(mype+1)%MAX_NUM_SENDS

!   WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): request ',L,' is ', linfo(mype+1)%MSG_REQS(L)
! END DO

      ! Complete sends and receives. 
      ! If recv, unpack into user space (domain face).

      DO L = 1, linfo(mype+1)%NUM_RECVS + linfo(mype+1)%NUM_SENDS

         TIME_START_DIR = MPI_WTIME()

         CALL MPI_WAITANY ( linfo(mype+1)%MAX_NUM_SENDS + linfo(mype+1)%MAX_NUM_RECVS, linfo(mype+1)%MSG_REQS, IWHICH, ISTAT, IERR )
         CALL CHECK_ERROR ( IERR, 'MG_UNPACK_AGG: MPI_WAITANY', ISTAT(MPI_ERROR), mype )

 !WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): waitany returned ',IWHICH

         IF ( IWHICH > linfo(mype+1)%MAX_NUM_RECVS ) THEN
            NUM_SENDS_OUTSTANDING = NUM_SENDS_OUTSTANDING - 1
            I = IWHICH - linfo(mype+1)%MAX_NUM_RECVS
            IF (I.EQ.BACK .OR. I.EQ.FRONT) THEN
               TIME_WAIT_Z = TIME_WAIT_Z + MPI_WTIME() - TIME_START_DIR
            ELSE IF (I.EQ.EAST .OR. I.EQ.WEST) THEN
               TIME_WAIT_X = TIME_WAIT_X + MPI_WTIME() - TIME_START_DIR
            ELSE IF (I.EQ.NORTH .OR. I.EQ.SOUTH) THEN
               TIME_WAIT_Y = TIME_WAIT_Y + MPI_WTIME() - TIME_START_DIR
            END IF

         ELSE 

            IF (IWHICH.EQ.BACK .OR. IWHICH.EQ.FRONT) THEN
               TIME_WAIT_Z = TIME_WAIT_Z + MPI_WTIME() - TIME_START_DIR
            ELSE IF (IWHICH.EQ.EAST .OR. IWHICH.EQ.WEST) THEN
               TIME_WAIT_X = TIME_WAIT_X + MPI_WTIME() - TIME_START_DIR
            ELSE IF (IWHICH.EQ.NORTH .OR. IWHICH.EQ.SOUTH) THEN
               TIME_WAIT_Y = TIME_WAIT_Y + MPI_WTIME() - TIME_START_DIR
            END IF
            TIME_START_DIR = MPI_WTIME()

            OFFSET = 0
            DO I = 1, NUM_VARS
               CALL MG_GET_FACE ( I, IWHICH, OFFSET, IERR, mype )
            END DO
            NUM_RECVS_OUTSTANDING = NUM_RECVS_OUTSTANDING - 1
            IF (IWHICH.EQ.BACK .OR. IWHICH.EQ.FRONT) THEN
               MG_PERF(mype+1)%TIME_UNPACK_Z_PE = MG_PERF(mype+1)%TIME_UNPACK_Z_PE +     &
                                          MPI_WTIME() - TIME_START_DIR
            ELSE IF (IWHICH.EQ.EAST .OR. IWHICH.EQ.WEST) THEN
               MG_PERF(mype+1)%TIME_UNPACK_X_PE = MG_PERF(mype+1)%TIME_UNPACK_X_PE +     &
                                          MPI_WTIME() - TIME_START_DIR
            ELSE IF (IWHICH.EQ.NORTH .OR. IWHICH.EQ.SOUTH) THEN
               MG_PERF(mype+1)%TIME_UNPACK_Y_PE = MG_PERF(mype+1)%TIME_UNPACK_Y_PE +     &
                                          MPI_WTIME() - TIME_START_DIR
            END IF

         END IF
      END DO

      CALL CHECK_ERROR ( NUM_SENDS_OUTSTANDING, 'MG_UNPACK_AGG: NUM_SENDS_OUTSTANDING',  &
                         linfo(mype+1)%NUM_SENDS, mype )
      CALL CHECK_ERROR ( NUM_RECVS_OUTSTANDING, 'MG_UNPACK_AGG: NUM_RECVS_OUTSTANDING',  &
                         linfo(mype+1)%NUM_RECVS, mype )

      linfo(mype+1)%NUM_RECVS = 0
      linfo(mype+1)%NUM_SENDS = 0

      TIME_WAIT = TIME_WAIT_X +  TIME_WAIT_Y + TIME_WAIT_Z
      MG_PERF(mype+1)%TIME_UNPACK_PE = MG_PERF(mype+1)%TIME_UNPACK_PE + MPI_WTIME ( ) - &
                               TIME_START - TIME_WAIT
      MG_PERF(mype+1)%TIME_WAIT_PE = MG_PERF(mype+1)%TIME_WAIT_PE + TIME_WAIT
      MG_PERF(mype+1)%TIME_WAIT_X_PE = MG_PERF(mype+1)%TIME_WAIT_X_PE + TIME_WAIT_X
      MG_PERF(mype+1)%TIME_WAIT_Y_PE = MG_PERF(mype+1)%TIME_WAIT_Y_PE + TIME_WAIT_Y
      MG_PERF(mype+1)%TIME_WAIT_Z_PE = MG_PERF(mype+1)%TIME_WAIT_Z_PE + TIME_WAIT_Z

      RETURN
   
   END SUBROUTINE MG_UNPACK_AGG
   
END MODULE MG_UNPACK_AGG_MOD
