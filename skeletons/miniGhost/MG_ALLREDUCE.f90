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

MODULE MG_ALLREDUCE_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Reduce for a single variable.

   ! Note: Using 0-based indexing on arrays.

CONTAINS

   SUBROUTINE MG_ALLREDUCE_SUM ( GSUM, IERR, mype )

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
     !    GRID_IN

      REAL(KIND=MG_REAL), INTENT(OUT) :: &
         GSUM                                ! Global value.

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

	
         integer :: mype

      ! ------------------
      ! Local Declarations
      ! ------------------

      REAL(KIND=MG_REAL) :: &
         LSUM                 ! Local GRID sum.

      REAL(KIND=MG_REAL8) ::      &
         TIME_START           ! Timer.

      INTEGER :: &
         I, J, K              ! Counters

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      TIME_START = MPI_WTIME()
      LSUM = 0.0
      !DO K = 1, NZ
      !   DO J = 1, NY
      !      DO I = 1, NX
              ! LSUM = LSUM + GRID_IN(I, J, K)
      !      END DO
      !   END DO
      !END DO
		call SSTMAC_compute_loop3(1, NZ, 1, NY, 1, NX, 1);

      MG_PERF(mype+1)%TIME_SUMGRID_COMP_PE = MG_PERF(mype+1)%TIME_SUMGRID_COMP_PE + MPI_WTIME() - TIME_START

      TIME_START = MPI_WTIME ( )
      CALL MPI_ALLREDUCE (  1, MG_MPI_REAL, MPI_SUM, linfo(mype+1)%MPI_COMM_MG, IERR )
      MG_PERF(mype+1)%TIME_SUMGRID_COMM_PE = MG_PERF(mype+1)%TIME_SUMGRID_COMM_PE + MPI_WTIME() - TIME_START

      IF ( PROFILE_MG ) THEN
         MG_PERF(mype+1)%NUM_ALLREDUCES = MG_PERF(mype+1)%NUM_ALLREDUCES + 1
         MG_PERF(mype+1)%ALLREDUCE_COUNT = MG_PERF(mype+1)%ALLREDUCE_COUNT + 1

         MG_PERF(mype+1)%ALLREDUCE_COUNT_MAX = 1   ! FIXME: If expand to allow COUNT > 1.
         MG_PERF(mype+1)%ALLREDUCE_COUNT_MIN = 1   ! FIXME: If expand to allow COUNT > 1.
      END IF

   END SUBROUTINE MG_ALLREDUCE_SUM

END MODULE MG_ALLREDUCE_MOD
