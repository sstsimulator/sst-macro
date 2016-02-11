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

MODULE MG_SUM_GRID_MOD

   USE MG_UTILS_MOD
   USE MG_ALLREDUCE_MOD

   IMPLICIT NONE

   ! Note: Using 0-based indexing on arrays.

CONTAINS

   SUBROUTINE MG_SUM_GRID ( IVAR, GSUM, IERR, mype )

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         IVAR           ! Variable to be reduced.

      REAL(KIND=MG_REAL), INTENT(OUT) :: & 
         GSUM

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

	
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE( IVAR )

         CASE (1)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (2)
            CALL MG_ALLREDUCE_SUM (GSUM, IERR, mype )
         CASE (3)
            CALL MG_ALLREDUCE_SUM ( GSUM, IERR, mype )
         CASE (4)
            CALL MG_ALLREDUCE_SUM ( GSUM, IERR, mype )
         CASE (5)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (6)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (7)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (8)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (9)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (10)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (11)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (12)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (13)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (14)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (15)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (16)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (17)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (18)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (19)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (20)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (21)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (22)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (23)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (24)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (25)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (26)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (27)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (28)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (29)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (30)
            CALL MG_ALLREDUCE_SUM ( GSUM, IERR, mype )
         CASE (31)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (32)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (33)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (34)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (35)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (36)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (37)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (38)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (39)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
         CASE (40)
            CALL MG_ALLREDUCE_SUM (  GSUM, IERR, mype )
      END SELECT

      IF ( PROFILE_MG ) THEN
         MG_PERF(mype+1)%NUM_SUMGRID = MG_PERF(mype+1)%NUM_SUMGRID + 1
      END IF

   END SUBROUTINE MG_SUM_GRID

END MODULE MG_SUM_GRID_MOD
