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

MODULE MG_SVAF_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_IRECV_MOD
   USE MG_PACK_MOD
   USE MG_SEND_SVAF_MOD
   USE MG_UNPACK_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_SVAF ( IVAR, IERR, mype )
   
      ! -------------------------------------------------------
      ! Pack all variables for single message to each neighbor.
      ! -------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         IVAR           ! Return status

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

            
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER :: &
         ICOUNT,                      &  ! Message size.
         ISTAT,                       &  ! MPI return status.
         IWHICH,                      &  ! MPI_Wait_any SRC process
         I, J, K, L, M, N,            &  ! Counters
         NUM_RECVS,                   &
         NUM_RECVS_OUTSTANDING,       &
         MSG_REQ_OFFSET,              &
         OFFSET,                      &
         OFFSET_LAST,                 &
         RECV_BUFFER_SIZE,            &
         SEND_BUFFER_SIZE,            &
         MSG_REQ(12)
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      linfo(mype+1)%COUNT_SEND_NORTH = 0
      linfo(mype+1)%COUNT_SEND_SOUTH = 0
      linfo(mype+1)%COUNT_SEND_EAST  = 0
      linfo(mype+1)%COUNT_SEND_WEST  = 0
      linfo(mype+1)%COUNT_SEND_BACK  = 0
      linfo(mype+1)%COUNT_SEND_FRONT = 0
   
      SELECT CASE ( IVAR )

         CASE ( 1 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 2 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 3 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 4 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK ( IERR, mype )
         CASE ( 5 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 6 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 7 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 8 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 9 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 10 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF ( IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
            
         CASE ( 11 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK ( IERR, mype )
         CASE ( 12 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 13 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF ( IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 14 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 15 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 16 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 17 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 18 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 19 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 20 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )

         CASE ( 21 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 22 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 23 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK ( IERR, mype )
         CASE ( 24 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 25 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 26 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 27 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 28 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 29 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 30 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
            
         CASE ( 31 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF ( IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 32 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 33 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 34 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 35 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK ( IERR, mype )
         CASE ( 36 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 37 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 38 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 39 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )
         CASE ( 40 )
            CALL MG_IRECV ( IERR, mype )
            CALL MG_PACK (  IERR, mype )
            CALL MG_SEND_SVAF (  IERR, mype )
            CALL MG_UNPACK (  IERR, mype )

         CASE DEFAULT
            WRITE(*,*) '** Error ** COMM_MG_SVAF: Unknown variable number ', IVAR, '.'

      END SELECT
            
   END SUBROUTINE MG_SVAF
   
END MODULE MG_SVAF_MOD
