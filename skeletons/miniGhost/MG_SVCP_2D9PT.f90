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

MODULE MG_SVCP_2D9PT_MOD

   USE MG_UTILS_MOD
   USE MG_GET_FACE_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

   ! Stencil for a single variable.

   ! Note: Using 0-based indexing on arrays.

CONTAINS
   
   SUBROUTINE MG_SVCP_2D9PT (  IERR )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
      !   GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         I, J, K,             & ! Counters
         NUM_GRID_PTS

      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      IF ( MYPE == ROOT_PE ) &
         WRITE(*,*) 'MG_SVCP_2D9PT not yet implemented'

      RETURN

      DO K = 1, NZ 
         DO J = 1, NY
            DO I = 1, NX
             !  WORK(I,J,K) = ( GRID(I-1,J-1,K) + GRID(I-1,J,K) + GRID(I-1,J+1,K) +  &
             !                  GRID(I  ,J-1,K) + GRID(I  ,J,K) + GRID(I  ,J+1,K) +  &
             !                  GRID(I+1,J-1,K) + GRID(I+1,J,K) + GRID(I+1,J+1,K) )  &
             !                      * NINTH
            END DO
         END DO

         call SSTMAC_compute_loop2(1, NY, 1, NX, 9)

         IF ( K == 1 ) THEN
            IF ( NEIGHBORS(BACK) /= -1 ) THEN
               CALL MPI_SEND ( NX*NY, MG_MPI_REAL, NEIGHBORS(BACK), MSG_TAGS(BACK),  &
                               MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_2D9PT: MPI_SEND(BACK)', I )

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
            IF ( NEIGHBORS(FRONT) /= -1 ) THEN
               CALL MPI_SEND ( NX*NY, MG_MPI_REAL, NEIGHBORS(FRONT), MSG_TAGS(FRONT),  &
                               MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_2D9PT: MPI_SEND(FRONT)', I )

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
            IF ( NEIGHBORS(EAST) /= -1 ) THEN
               CALL MPI_SEND ( NY, MG_MPI_REAL, NEIGHBORS(EAST), MSG_TAGS(EAST)+I,        &
                               MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_2D9PT: MPI_SEND(EAST)', I )

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

            IF ( NEIGHBORS(WEST) /= -1 ) THEN
               CALL MPI_SEND ( NY, MG_MPI_REAL, NEIGHBORS(WEST), MSG_TAGS(WEST)+I,       &
                               MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_2D9PT: MPI_SEND(WEST)', I )
     
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

            IF ( NEIGHBORS(NORTH) /= -1 ) THEN
               ! Pack into NX length buffers.
               DO I = 1, NX
                  SEND_BUFFER ( I ) = WORK ( I, NY, K )
               END DO
               CALL MPI_SEND ( NX, MG_MPI_REAL, NEIGHBORS(NORTH), MSG_TAGS(NORTH)+K,                &
                               MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_2D9PT: MPI_SEND(NORTH)', I )
     
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
      
            IF ( NEIGHBORS(SOUTH) /= -1 ) THEN
               ! Pack into NX length buffers.
               DO I = 1, NX
                  SEND_BUFFER ( I ) = WORK ( I, 1, K )
               END DO
               NUM_SENDS = NUM_SENDS + 1
               CALL MPI_SEND ( NX, MG_MPI_REAL, NEIGHBORS(SOUTH), MSG_TAGS(SOUTH)+K,  &
                               MPI_COMM_MG, IERR )
               CALL CHECK_ERROR ( IERR, 'MG_SVCP_2D9PT: MPI_SEND(SOUTH)', I )
     
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

      END DO

      call SSTMAC_compute_loop(0, NZ, 5)

      DO K = 1, NZ
         DO J = 1, NY
            DO I = 1, NX
               GRID(I,J,K) = WORK(I,J,K)
            END DO
         END DO
      END DO

      call SSTMAC_compute_loop3(1, NZ, 1, NY, 1, NX, 1)

      IF ( PROFILE_MG ) THEN
         NUM_GRID_PTS = NX*NY*NZ
         MG_PERF(mype+1)%NUM_ADDS  = MG_PERF(mype+1)%NUM_ADDS  + 8*(NUM_GRID_PTS)
         MG_PERF(mype+1)%NUM_MULTS = MG_PERF(mype+1)%NUM_MULTS + NUM_GRID_PTS
      END IF

   END SUBROUTINE MG_SVCP_2D9PT
   
END MODULE MG_SVCP_2D9PT_MOD
