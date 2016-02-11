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

MODULE MG_BSPMA_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_IRECV_MOD
   USE MG_PACK_MOD
   USE MG_SEND_MOD
   USE MG_UNPACK_AGG_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_BSPMA ( IERR, mype )
   
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
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------
	WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): MG_BSPMA'
      IERR = 0

      CALL MG_IRECV ( IERR, mype )

      linfo(mype+1)%COUNT_SEND_NORTH = 0
      linfo(mype+1)%COUNT_SEND_SOUTH = 0
      linfo(mype+1)%COUNT_SEND_EAST  = 0
      linfo(mype+1)%COUNT_SEND_WEST  = 0
      linfo(mype+1)%COUNT_SEND_BACK  = 0
      linfo(mype+1)%COUNT_SEND_FRONT = 0
 
      IF ( NUM_VARS > 0 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 1 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 2 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 3 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 4 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 5 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 6 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 7 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 8 ) &
         CALL MG_PACK (   IERR, mype )
      IF ( NUM_VARS > 9 ) &
         CALL MG_PACK (  IERR, mype )

      IF ( NUM_VARS > 10 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 11 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 12 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 13 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 14 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 15 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 16 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 17 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 18 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 19 ) &
         CALL MG_PACK (  IERR, mype )

      IF ( NUM_VARS > 20 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 21 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 22 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 23 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 24 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 25 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 26 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 27 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 28 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 29 ) &
         CALL MG_PACK (  IERR, mype )

      IF ( NUM_VARS > 30 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 31 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 32 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 33 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 34 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 35 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 36 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 37 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 38 ) &
         CALL MG_PACK (  IERR, mype )
      IF ( NUM_VARS > 39 ) &
         CALL MG_PACK (  IERR, mype )

      CALL MG_SEND ( IERR, mype )

      CALL MG_UNPACK_AGG ( IERR, mype )

   END SUBROUTINE MG_BSPMA
   
END MODULE MG_BSPMA_MOD
