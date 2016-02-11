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

MODULE MG_BC_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! MG_BC_HN : Homogeneous Neumann bounardy conditions.
   ! MG_BC_HD : Homogeneous Dirichlet bounardy conditions.

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_BC_HN ( GRID, IERR )

      ! Homogeneous Neumann boundary condition.
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
         GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         I, J, K            ! Counters

      REAL(KIND=MG_REAL8) ::         &
         TIME_START

      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      TIME_START = MPI_WTIME ( )

      IF ( NEIGHBORS(NORTH) == -1 ) THEN
         DO K = 1, NZ
            DO J = 1, NY
               GRID(0,J,K) = GRID(1,J,K)
            END DO
         END DO
         call SSTMAC_compute_loop2(1, NZ, 1, NY, 1)
      END IF

      IF ( NEIGHBORS(SOUTH) == -1 ) THEN
         DO K = 1, NZ
            DO J = 1, NY
               GRID(NX+1,J,K) = GRID(NX,J,K)
            END DO
         END DO
         call SSTMAC_compute_loop2(1, NZ, 1, NY, 1)
      END IF

      IF ( NEIGHBORS(EAST) == -1 ) THEN
         DO K = 1, NZ
            DO I = 1, NX
               GRID(I,NY+1,K) = GRID(I,NY,K)
            END DO
         END DO
         call SSTMAC_compute_loop2(1, NZ, 1, NX, 1)
      END IF

      IF ( NEIGHBORS(WEST) == -1 ) THEN
         DO K = 1, NZ
            DO I = 1, NX
               GRID(I,0,K) = GRID(I,1,K)
            END DO
         END DO
         call SSTMAC_compute_loop2(1, NZ, 1, NX, 1)
      END IF

      IF ( NEIGHBORS(BACK) == -1 ) THEN
         DO J = 1, NY
            DO I = 1, NX
               GRID(I,J,0) = GRID(I,J,1)
            END DO
         END DO
         call SSTMAC_compute_loop2(1, NX, 1, NY, 1)
      END IF

      IF ( NEIGHBORS(FRONT) == -1 ) THEN
         DO J = 1, NY
            DO I = 1, NX
               GRID(I,J,NZ+1) = GRID(I,J,NZ)
            END DO
         END DO
         call SSTMAC_compute_loop2(1, NY, 1, NY, 1)
      END IF

      IF ( PROFILE_MG ) THEN
         MG_PERF(mype+1)%TIME_BC_PE = MG_PERF(mype+1)%TIME_BC_PE + MPI_WTIME ( ) - TIME_START
      END IF

   END SUBROUTINE MG_BC_HN
   
END MODULE MG_BC_MOD
