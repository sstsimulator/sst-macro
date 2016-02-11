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

MODULE MG_GET_FACE_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD

   IMPLICIT NONE

CONTAINS

   SUBROUTINE GET_FACE ( WHICH, OFFSET,  IERR, mype )

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         WHICH

      INTEGER, INTENT(INOUT) :: &
         OFFSET

      INTEGER, INTENT(OUT) :: &
         IERR, mype

         

     ! REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(OUT) :: &
     !    GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         I, J, K                  ! Counters

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE ( WHICH )

         CASE ( BACK )

            DO J = 0, NY+1
               DO I = 0, NX+1
                  OFFSET = OFFSET + 1
                 ! GRID ( I, J, 0 ) = linfo(mype+1)%RECV_BUFFER_BACK ( OFFSET )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NX+1, 2)

         CASE ( FRONT )

            DO J = 0, NY+1
               DO I = 0, NX+1
                  OFFSET = OFFSET + 1
                !  GRID ( I, J, NZ+1 ) = linfo(mype+1)%RECV_BUFFER_FRONT ( OFFSET )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NX+1, 2)

         CASE ( EAST )

            DO K = 0, NZ+1
               DO J = 0, NY+1
                  OFFSET = OFFSET + 1
                !  GRID ( NX+1, J, K ) = linfo(mype+1)%RECV_BUFFER_EAST ( OFFSET )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NZ+1, 2)

         CASE ( WEST )

            DO K = 0, NZ+1
               DO J = 0, NY+1
                  OFFSET = OFFSET + 1
                 ! GRID ( 0, J, K ) = linfo(mype+1)%RECV_BUFFER_WEST ( OFFSET )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NY+1, 0, NZ+1, 2)

         CASE ( NORTH )

            DO K = 0, NZ+1
               DO I = 0, NX+1
                  OFFSET = OFFSET + 1
                !  GRID ( I, NY+1, K ) = linfo(mype+1)%RECV_BUFFER_NORTH ( OFFSET )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)

         CASE ( SOUTH )

            DO K = 0, NZ+1
               DO I = 0, NX+1
                  OFFSET = OFFSET + 1
                 ! GRID ( I, 0, K ) = linfo(mype+1)%RECV_BUFFER_SOUTH ( OFFSET )
               END DO
            END DO
            call SSTMAC_compute_loop2(0, NZ+1, 0, NX+1, 2)

      END SELECT

   END SUBROUTINE GET_FACE

   SUBROUTINE MG_GET_FACE ( IVAR, WHICH, OFFSET, IERR, mype )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) :: &
         IVAR,                             &  ! Which variable.
         WHICH                                ! Which boundary (north, south, etc)

      INTEGER(KIND=MG_INT), INTENT(INOUT) :: &
         OFFSET

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR, mype           ! Return status

         

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER(KIND=MG_INT) :: &
         I, J                            ! Counters
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE ( IVAR )

         CASE ( 1 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 2 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 3 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 4 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 5 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 6 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 7 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 8 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 9 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 10 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 11 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 12 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 13 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 14 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 15 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 16 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 17 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 18 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 19 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 20 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 21 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 22 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 23 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 24 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 25 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 26 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 27 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 28 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 29 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 30 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 31 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 32 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 33 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 34 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 35 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 36 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 37 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 38 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 39 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

         CASE ( 40 )

            CALL GET_FACE ( WHICH, OFFSET,  IERR, mype )

      END SELECT

      RETURN
   
   END SUBROUTINE MG_GET_FACE

END MODULE MG_GET_FACE_MOD
