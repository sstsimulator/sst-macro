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

MODULE MG_SEND_SVAF_MOD

   USE MG_CONSTANTS_MOD
   USE MG_ISEND_SVAF_MOD
   USE MG_BSEND_SVAF_MOD

   IMPLICIT NONE

   ! 

CONTAINS
   
   SUBROUTINE MG_SEND_SVAF (  IERR, mype )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(IN) :: &
     !    GRID

    
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER :: &
         I, J,                    &  ! Counters
         MSG_REQS_OFFSET
   
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

      SELECT CASE ( SEND_PROTOCOL )

         CASE ( SEND_PROTOCOL_BLOCKING )

            CALL MG_BSEND_SVAF (  IERR, mype )

         CASE ( SEND_PROTOCOL_NONBLOCKING )

            CALL MG_ISEND_SVAF (  IERR, mype )

         CASE DEFAULT

            IERR = -1
            CALL CHECK_ERROR ( IERR, 'MG_SEND_SVAF: Unknown SEND_PROTOCOL', SEND_PROTOCOL, mype )
      END SELECT

   END SUBROUTINE MG_SEND_SVAF
   
END MODULE MG_SEND_SVAF_MOD
