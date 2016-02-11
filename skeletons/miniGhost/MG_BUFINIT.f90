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

MODULE MG_BUFINIT_MOD

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD

   IMPLICIT NONE

CONTAINS
   
   SUBROUTINE MG_BUFINIT ( IERR, mype )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR, mype           ! Return status

         

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         I                      ! Counter

      REAL :: &
         NUM,               &  ! Tmp random number.
         PERCENT_SUM_REAL      ! Requested percentage of GRIDS_TO_SUM
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0


 	WRITE(*,*) 'mini_ghost(',mype,'): MG_BUFINIT'

      ! ---------------------------
      ! Reduction sum across GRIDs.
      ! ---------------------------

      ALLOCATE ( linfo(mype+1)%GRIDS_TO_SUM(NUM_VARS), STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'MG_BUFINFO: ALLOCATE(GRIDS_TO_SUM)', NUM_VARS, mype )

      IF ( CHECK_DIFFUSION ) THEN
         PERCENT_SUM = 100 ! If we're checking, we're checking them all.
      END IF

      IF ( PERCENT_SUM == 100 ) THEN

         linfo(mype+1)%GRIDS_TO_SUM = .TRUE.    ! Every GRID is summed.
         linfo(mype+1)%NUM_SUM_GRID = NUM_VARS

      ELSE IF ( PERCENT_SUM == 0 ) THEN

         linfo(mype+1)%GRIDS_TO_SUM = .FALSE.   ! No GRIDs are summed.
         linfo(mype+1)%NUM_SUM_GRID = 0

      ELSE

         PERCENT_SUM_REAL = REAL(PERCENT_SUM) / 100.0
         linfo(mype+1)%NUM_SUM_GRID = 0

         DO I = 1, NUM_VARS
            CALL RANDOM_NUMBER ( NUM )
            IF ( NUM < PERCENT_SUM ) THEN
               linfo(mype+1)%GRIDS_TO_SUM(I) = .TRUE.
               linfo(mype+1)%NUM_SUM_GRID = linfo(mype+1)%NUM_SUM_GRID + 1
            ELSE
               linfo(mype+1)%GRIDS_TO_SUM(I) = .FALSE.
            END IF
         END DO

         call SSTMAC_compute_loop(1, NUM_VARS, 3)

      END IF

#if !defined ( _MG_DEBUG )
      IF ( NUMPES == 1 ) &
         RETURN
#endif

      SELECT CASE ( COMM_METHOD )

         CASE ( COMM_METHOD_BSPMA )

            CALL MG_BUF_BSPMA ( IERR, mype )

         CASE ( COMM_METHOD_SVAF )

            CALL MG_BUF_SVAF ( IERR, mype )

         CASE ( COMM_METHOD_SVCP )

            CALL MG_BUF_SVCP ( IERR, mype )

         CASE DEFAULT

            CALL CHECK_ERROR ( -1, 'MG_BUFINIT: Unknown COMM_METHOD', COMM_METHOD, mype )

      END SELECT

      ! -----------------
      ! Set message tags.
      ! -----------------

      ALLOCATE ( linfo(mype+1)%MSG_TAGS(MAX_NUM_NEIGHBORS), STAT = IERR )
      CALL CHECK_ERROR ( IERR, 'MG_BUFINFO: ALLOCATE(MSG_TAGS)', MAX_NUM_NEIGHBORS, mype )

      linfo(mype+1)%MSG_TAGS(NORTH) = 1000
      linfo(mype+1)%MSG_TAGS(SOUTH) = 2000
      linfo(mype+1)%MSG_TAGS(EAST)  = 3000
      linfo(mype+1)%MSG_TAGS(WEST)  = 4000
      linfo(mype+1)%MSG_TAGS(BACK)  = 5000
      linfo(mype+1)%MSG_TAGS(FRONT) = 6000

      ! -----------------------------
      ! Set msg request handle space.
      ! -----------------------------

      ALLOCATE ( linfo(mype+1)%MSG_REQS(linfo(mype+1)%MAX_NUM_SENDS+linfo(mype+1)%MAX_NUM_RECVS), STAT = IERR )
      CALL CHECK_ERROR ( IERR, 'MG_BUFINFO: ALLOCATE(MSG_REQS)', &
                         linfo(mype+1)%MAX_NUM_SENDS+linfo(mype+1)%MAX_NUM_RECVS, mype )

     linfo(mype+1)%MSG_REQS(1:linfo(mype+1)%MAX_NUM_SENDS+linfo(mype+1)%MAX_NUM_RECVS) = MPI_REQUEST_NULL

   END SUBROUTINE MG_BUFINIT

!  ========================================================================================
   
   SUBROUTINE MG_BUF_BSPMA ( IERR, mype )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR, mype           ! Return status

         

      ! ------------------
      ! Local Declarations
      ! ------------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------
	WRITE(*,*) 'mini_ghost(',mype,'): MG_BUF_BSPMA'
      IERR = 0

#if !defined ( _MG_DEBUG )
      IF ( NUMPES == 1 ) &
         RETURN
#endif

      IF ( SEND_PROTOCOL == SEND_PROTOCOL_BLOCKING ) &
         CALL CHECK_ERROR ( -1, 'MG_BUF_BSPMA: Lets rethink blocking send for BSPMA )', -1, mype )

      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN
	WRITE(*,*) 'mini_ghost(',mype,'): allocating BACK'
         linfo(mype+1)%SEND_BUFFER_BACK_SIZE = NUM_VARS * (NX+2)*(NY+2)
         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_BACK( linfo(mype+1)%SEND_BUFFER_BACK_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_BACK', linfo(mype+1)%SEND_BUFFER_BACK_SIZE, mype )

         linfo(mype+1)%RECV_BUFFER_BACK_SIZE = NUM_VARS * (NX+2)*(NY+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_BACK( linfo(mype+1)%RECV_BUFFER_BACK_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_BACK', linfo(mype+1)%RECV_BUFFER_BACK_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN
	WRITE(*,*) 'mini_ghost(',mype,'): allocating FRONT'
         linfo(mype+1)%SEND_BUFFER_FRONT_SIZE = NUM_VARS * (NX+2)*(NY+2)
         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_FRONT( linfo(mype+1)%SEND_BUFFER_FRONT_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_FRONT', linfo(mype+1)%SEND_BUFFER_FRONT_SIZE, mype )

         linfo(mype+1)%RECV_BUFFER_FRONT_SIZE = NUM_VARS * (NX+2)*(NY+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_FRONT( linfo(mype+1)%RECV_BUFFER_FRONT_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_FRONT', linfo(mype+1)%RECV_BUFFER_FRONT_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN
	WRITE(*,*) 'mini_ghost(',mype,'): allocating EAST'
         linfo(mype+1)%SEND_BUFFER_EAST_SIZE = NUM_VARS * (NY+2)*(NZ+2)
        !  ALLOCATE ( linfo(mype+1)%SEND_BUFFER_EAST( linfo(mype+1)%SEND_BUFFER_EAST_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_EAST', linfo(mype+1)%SEND_BUFFER_EAST_SIZE, mype )

         linfo(mype+1)%RECV_BUFFER_EAST_SIZE = NUM_VARS * (NY+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_EAST( linfo(mype+1)%RECV_BUFFER_EAST_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_EAST', linfo(mype+1)%RECV_BUFFER_EAST_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN
	WRITE(*,*) 'mini_ghost(',mype,'): allocating WEST'
         linfo(mype+1)%SEND_BUFFER_WEST_SIZE = NUM_VARS * (NY+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_WEST( linfo(mype+1)%SEND_BUFFER_WEST_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_WEST', linfo(mype+1)%SEND_BUFFER_WEST_SIZE, mype )

         linfo(mype+1)%RECV_BUFFER_WEST_SIZE = NUM_VARS * (NY+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_WEST( linfo(mype+1)%RECV_BUFFER_WEST_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_WEST', linfo(mype+1)%RECV_BUFFER_WEST_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN
	WRITE(*,*) 'mini_ghost(',mype,'): allocating NORTH'
         linfo(mype+1)%SEND_BUFFER_NORTH_SIZE = NUM_VARS * (NX+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_NORTH( linfo(mype+1)%SEND_BUFFER_NORTH_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_NORTH', linfo(mype+1)%SEND_BUFFER_NORTH_SIZE, mype )

         linfo(mype+1)%RECV_BUFFER_NORTH_SIZE = NUM_VARS * (NX+2)*(NZ+2)
         ALLOCATE ( linfo(mype+1)%RECV_BUFFER_NORTH( linfo(mype+1)%RECV_BUFFER_NORTH_SIZE ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_NORTH', linfo(mype+1)%RECV_BUFFER_NORTH_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN
	WRITE(*,*) 'mini_ghost(',mype,'): allocating SOUTH'
         linfo(mype+1)%SEND_BUFFER_SOUTH_SIZE = NUM_VARS * (NX+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_SOUTH( linfo(mype+1)%SEND_BUFFER_SOUTH_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_SOUTH', linfo(mype+1)%SEND_BUFFER_SOUTH_SIZE, mype )

         linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE = NUM_VARS * (NX+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_SOUTH( linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_SOUTH', linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE, mype )

      END IF

      linfo(mype+1)%MAX_NUM_SENDS = MAX_NUM_NEIGHBORS
      linfo(mype+1)%MAX_NUM_RECVS = MAX_NUM_NEIGHBORS

   END SUBROUTINE MG_BUF_BSPMA

!  ========================================================================================
   
   SUBROUTINE MG_BUF_SVAF ( IERR, mype )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR, mype           ! Return status

         

      ! ------------------
      ! Local Declarations
      ! ------------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------
	WRITE(*,*) 'mini_ghost(',mype,'): MG_BUF_SVAF'
      IERR = 0

#if !defined ( _MG_DEBUG )
      IF ( NUMPES == 1 ) &
         RETURN
#endif

      IF ( SEND_PROTOCOL == SEND_PROTOCOL_BLOCKING ) THEN

         ! Share buffer.
         linfo(mype+1)%SEND_BUFFER_SIZE = NX
         IF ( NY > linfo(mype+1)%SEND_BUFFER_SIZE ) &
            linfo(mype+1)%SEND_BUFFER_SIZE = NY
         IF ( NZ > linfo(mype+1)%SEND_BUFFER_SIZE ) &
            linfo(mype+1)%SEND_BUFFER_SIZE = NZ

         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER ( linfo(mype+1)%SEND_BUFFER_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( -1, 'MG_BUF_SVAF: SEND_BUFFER ( SEND_PROTOCOL_BLOCKING )', &
                  !           linfo(mype+1)%SEND_BUFFER_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(BACK) /= -1 ) THEN

         IF ( SEND_PROTOCOL /= SEND_PROTOCOL_BLOCKING ) THEN
            linfo(mype+1)%SEND_BUFFER_BACK_SIZE = (NX+2)*(NY+2)
            ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_BACK( linfo(mype+1)%SEND_BUFFER_BACK_SIZE ), STAT = IERR )
            ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_BACK', linfo(mype+1)%SEND_BUFFER_BACK_SIZE, mype )
         END IF

        linfo(mype+1)% RECV_BUFFER_BACK_SIZE = (NX+2)*(NY+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_BACK(linfo(mype+1)% RECV_BUFFER_BACK_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_BACK', linfo(mype+1)%RECV_BUFFER_BACK_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(FRONT) /= -1 ) THEN

         IF ( SEND_PROTOCOL /= SEND_PROTOCOL_BLOCKING ) THEN
            linfo(mype+1)%SEND_BUFFER_FRONT_SIZE = (NX+2)*(NY+2)
            ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_FRONT( linfo(mype+1)%SEND_BUFFER_FRONT_SIZE ), STAT = IERR )
            ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_FRONT', linfo(mype+1)%SEND_BUFFER_FRONT_SIZE, mype )
         END IF

         linfo(mype+1)%RECV_BUFFER_FRONT_SIZE = (NX+2)*(NY+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_FRONT( linfo(mype+1)%RECV_BUFFER_FRONT_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_FRONT', linfo(mype+1)%RECV_BUFFER_FRONT_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(EAST) /= -1 ) THEN

         IF ( SEND_PROTOCOL /= SEND_PROTOCOL_BLOCKING ) THEN
            linfo(mype+1)%SEND_BUFFER_EAST_SIZE =  (NY+2)*(NZ+2)
            ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_EAST( linfo(mype+1)%SEND_BUFFER_EAST_SIZE ), STAT = IERR )
            ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_EAST', linfo(mype+1)%SEND_BUFFER_EAST_SIZE, mype )
         END IF

         linfo(mype+1)%RECV_BUFFER_EAST_SIZE =  (NY+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_EAST( linfo(mype+1)%RECV_BUFFER_EAST_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_EAST', linfo(mype+1)%RECV_BUFFER_EAST_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(WEST) /= -1 ) THEN

         IF ( SEND_PROTOCOL /= SEND_PROTOCOL_BLOCKING ) THEN
            linfo(mype+1)%SEND_BUFFER_WEST_SIZE =  (NY+2)*(NZ+2)
            ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_WEST( linfo(mype+1)%SEND_BUFFER_WEST_SIZE ), STAT = IERR )
            ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_WEST', linfo(mype+1)%SEND_BUFFER_WEST_SIZE, mype )
         END IF

         linfo(mype+1)%RECV_BUFFER_WEST_SIZE =  (NY+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_WEST( linfo(mype+1)%RECV_BUFFER_WEST_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_WEST', linfo(mype+1)%RECV_BUFFER_WEST_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(NORTH) /= -1 ) THEN

         IF ( SEND_PROTOCOL /= SEND_PROTOCOL_BLOCKING ) THEN
            linfo(mype+1)%SEND_BUFFER_NORTH_SIZE = (NX+2)*(NZ+2)
            ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_NORTH( linfo(mype+1)%SEND_BUFFER_NORTH_SIZE ), STAT = IERR )
            ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_NORTH', linfo(mype+1)%SEND_BUFFER_NORTH_SIZE, mype )
         END IF

         linfo(mype+1)%RECV_BUFFER_NORTH_SIZE = (NX+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_NORTH( linfo(mype+1)%RECV_BUFFER_NORTH_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_NORTH', linfo(mype+1)%RECV_BUFFER_NORTH_SIZE, mype )

      END IF

      IF ( linfo(mype+1)%NEIGHBORS(SOUTH) /= -1 ) THEN

         IF ( SEND_PROTOCOL /= SEND_PROTOCOL_BLOCKING ) THEN
            linfo(mype+1)%SEND_BUFFER_SOUTH_SIZE = (NX+2)*(NZ+2)
            ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_SOUTH( linfo(mype+1)%SEND_BUFFER_SOUTH_SIZE ), STAT = IERR )
            ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_SOUTH', linfo(mype+1)%SEND_BUFFER_SOUTH_SIZE, mype )
         END IF

         linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE = (NX+2)*(NZ+2)
         ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_SOUTH( linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_SOUTH', linfo(mype+1)%RECV_BUFFER_SOUTH_SIZE, mype )

      END IF

      linfo(mype+1)%MAX_NUM_SENDS = MAX_NUM_NEIGHBORS
      linfo(mype+1)%MAX_NUM_RECVS = MAX_NUM_NEIGHBORS

#if defined (_DEBUG ) || ( _MG_DEBUG )
      print *,'SIZE(MSG_REQS)=', SIZE(MSG_REQS), MAX_NUM_SENDS, MAX_NUM_RECVS
#endif

   END SUBROUTINE MG_BUF_SVAF

!  ========================================================================================

   SUBROUTINE MG_BUF_SVCP ( IERR, mype )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR, mype           ! Return status

	
      ! ------------------
      ! Local Declarations
      ! ------------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------
	WRITE(*,*) 'mini_ghost(',mype,'): MG_BUF_SVCP'
      IERR = 0

      ! -----------------
      ! Set message tags.
      ! -----------------

      ! Currently only configured for BLOCKING sends.

      SEND_PROTOCOL = SEND_PROTOCOL_BLOCKING
 
      ! North and south buffers:
      ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_NORTH ( (NX+2)*(NZ+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER)', (NX+2)*(NZ+2), mype )

      ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_SOUTH ( (NX+2)*(NZ+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER)', (NX+2)*(NZ+2), mype )

      IF ( SEND_PROTOCOL == SEND_PROTOCOL_BLOCKING ) THEN

         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER ( NX ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER)', NX, mype )

      ELSE

         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_NORTH ( (NX+2)*(NY+2) ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER_NORTH)', (NX+2)*(NY+2), mype )

         ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_SOUTH ( (NX+2)*(NY+2) ), STAT = IERR )
         ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER_SOUTH)', (NX+2)*(NY+2), mype )

      END IF

      ! East and west buffers:

      ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_EAST( (NY+2)*(NZ+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: RECV_BUFFER_EAST)', (NX+2)*(NY+2), mype )

      ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_WEST( (NY+2)*(NZ+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: RECV_BUFFER_WEST)', (NX+2)*(NY+2), mype )

      ! Front and back buffers:
      ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_BACK ( (NX+2)*(NY+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: RECV_BUFFER_BACK)', (NX+2)*(NY+2), mype )

      ! ALLOCATE ( linfo(mype+1)%RECV_BUFFER_FRONT ( (NX+2)*(NY+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: RECV_BUFFER_FRONT)', (NX+2)*(NY+2), mype )

      ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_BACK ( (NX+2)*(NY+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER_BACK)', (NX+2)*(NY+2), mype )

      ! ALLOCATE ( linfo(mype+1)%SEND_BUFFER_FRONT ( (NX+2)*(NY+2) ), STAT = IERR )
      ! CALL CHECK_ERROR ( IERR, 'ALLOCATE ( MG_BUF_SVCP: SEND_BUFFER_FRONT)', (NX+2)*(NY+2), mype )

      ! Request handle offsets:
      linfo(mype+1)%REQ_OFFSET_RECV_BACK  = 1
      linfo(mype+1)%REQ_OFFSET_RECV_FRONT = 2
      linfo(mype+1)%REQ_OFFSET_RECV_EAST  = linfo(mype+1)%REQ_OFFSET_RECV_FRONT + NX
      linfo(mype+1)%REQ_OFFSET_RECV_WEST  = linfo(mype+1)%REQ_OFFSET_RECV_EAST  + NZ
      linfo(mype+1)%REQ_OFFSET_RECV_NORTH = linfo(mype+1)%REQ_OFFSET_RECV_WEST  + NZ
      linfo(mype+1)%REQ_OFFSET_RECV_SOUTH = linfo(mype+1)%REQ_OFFSET_RECV_NORTH + NZ

      linfo(mype+1)%MAX_NUM_RECVS = linfo(mype+1)%REQ_OFFSET_RECV_SOUTH + NZ

      linfo(mype+1)%MAX_NUM_SENDS = 0

   END SUBROUTINE MG_BUF_SVCP

END MODULE MG_BUFINIT_MOD

