! ************************************************************************
!
!               miniGhost: stencil computations with boundary exchange.
!                 Copyright (2011) Sandia Corporation
!
! Under terms of Contract DE-AC04-141AL85000, there is a non-exclusive
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

MODULE MG_PROFILING_MOD

      USE MG_CONSTANTS_MOD
      USE MG_UTILS_MOD

      IMPLICIT NONE

   ! Keep track of computation and communication. 

      INTEGER(KIND=MG_INT8) :: PERF_INIT_MIN = 999999999

      TYPE :: PERF_CHECK

         ! Counts are accumulated for each iteration, times and number of function calls are global.
         ! (This is designed to avoid overflow.)

         ! Local values:

         INTEGER(KIND=MG_INT8) :: NUM_COPY
         INTEGER(KIND=MG_INT8) :: NUM_ADDS
         INTEGER(KIND=MG_INT8) :: NUM_MULTS

         INTEGER(KIND=MG_INT8) :: NUM_SENDS
         INTEGER(KIND=MG_INT8) :: SEND_COUNT
         INTEGER(KIND=MG_INT8) :: SEND_COUNT_MAX
         INTEGER(KIND=MG_INT8) :: SEND_COUNT_MIN

         INTEGER(KIND=MG_INT8) :: NUM_RECVS
         INTEGER(KIND=MG_INT8) :: RECV_COUNT
         INTEGER(KIND=MG_INT8) :: RECV_COUNT_MAX
         INTEGER(KIND=MG_INT8) :: RECV_COUNT_MIN

         INTEGER(KIND=MG_INT8) :: NUM_BCASTS
         INTEGER(KIND=MG_INT8) :: BCAST_COUNT
         INTEGER(KIND=MG_INT8) :: BCAST_COUNT_MAX
         INTEGER(KIND=MG_INT8) :: BCAST_COUNT_MIN

         INTEGER(KIND=MG_INT8) :: NUM_ALLREDUCES
         INTEGER(KIND=MG_INT8) :: ALLREDUCE_COUNT
         INTEGER(KIND=MG_INT8) :: ALLREDUCE_COUNT_MAX
         INTEGER(KIND=MG_INT8) :: ALLREDUCE_COUNT_MIN

         INTEGER(KIND=MG_INT8) :: NUM_SUMGRID

         ! Timing variables:

         REAL(KIND=MG_REAL8) :: TIME_BSPMA_PE
         REAL(KIND=MG_REAL8) :: TIME_COPY_PE
         REAL(KIND=MG_REAL8) :: TIME_PACK_PE
         REAL(KIND=MG_REAL8) :: TIME_RECV_PE
         REAL(KIND=MG_REAL8) :: TIME_SEND_PE
         REAL(KIND=MG_REAL8) :: TIME_STENCIL_PE
         REAL(KIND=MG_REAL8) :: TIME_BC_PE
         REAL(KIND=MG_REAL8) :: TIME_SVAF_PE
         REAL(KIND=MG_REAL8) :: TIME_UNPACK_PE
         REAL(KIND=MG_REAL8) :: TIME_WAIT_PE
         REAL(KIND=MG_REAL8) :: TIME_WALL_PE

         REAL(KIND=MG_REAL8) :: TIME_PACK_X_PE
         REAL(KIND=MG_REAL8) :: TIME_SEND_X_PE
         REAL(KIND=MG_REAL8) :: TIME_WAIT_X_PE
         REAL(KIND=MG_REAL8) :: TIME_RECV_X_PE
         REAL(KIND=MG_REAL8) :: TIME_UNPACK_X_PE

         REAL(KIND=MG_REAL8) :: TIME_PACK_Y_PE
         REAL(KIND=MG_REAL8) :: TIME_SEND_Y_PE
         REAL(KIND=MG_REAL8) :: TIME_WAIT_Y_PE
         REAL(KIND=MG_REAL8) :: TIME_RECV_Y_PE
         REAL(KIND=MG_REAL8) :: TIME_UNPACK_Y_PE

         REAL(KIND=MG_REAL8) :: TIME_PACK_Z_PE
         REAL(KIND=MG_REAL8) :: TIME_SEND_Z_PE
         REAL(KIND=MG_REAL8) :: TIME_WAIT_Z_PE
         REAL(KIND=MG_REAL8) :: TIME_RECV_Z_PE
         REAL(KIND=MG_REAL8) :: TIME_UNPACK_Z_PE

         REAL(KIND=MG_REAL8) :: TIME_SVCP_PE

         REAL(KIND=MG_REAL8) :: TIME_SUMGRID_PE     ! Local summation + global summation.
         REAL(KIND=MG_REAL8) :: TIME_SUMGRID_COMP_PE
         REAL(KIND=MG_REAL8) :: TIME_SUMGRID_COMM_PE

      END TYPE PERF_CHECK

	TYPE(PERF_CHECK), dimension(:), allocatable :: MG_PERF

   CONTAINS

!     ==================================================================

      SUBROUTINE PERF_INIT ( IERR, mype )

         IMPLICIT NONE

         ! ----------------
         ! Scalar Arguments
         ! ----------------

         INTEGER, INTENT(OUT) :: IERR

         integer :: mype

         ! ---------------------
         ! Executable Statements
         ! ---------------------

	WRITE(*,*) 'mini_ghost(',mype,'): perf_init()'

         MG_PERF(mype+1)%NUM_ADDS        = 0
         MG_PERF(mype+1)%NUM_MULTS       = 0
         MG_PERF(mype+1)%NUM_COPY        = 0

         MG_PERF(mype+1)%NUM_SENDS       = 0
         MG_PERF(mype+1)%SEND_COUNT      = 0
         MG_PERF(mype+1)%NUM_RECVS       = 0
         MG_PERF(mype+1)%RECV_COUNT      = 0
         MG_PERF(mype+1)%NUM_BCASTS      = 0
         MG_PERF(mype+1)%BCAST_COUNT     = 0
         MG_PERF(mype+1)%NUM_ALLREDUCES  = 0
         MG_PERF(mype+1)%ALLREDUCE_COUNT = 0

         MG_PERF(mype+1)%NUM_SUMGRID     = 0

         MG_PERF(mype+1)%SEND_COUNT_MAX      = 0
         MG_PERF(mype+1)%RECV_COUNT_MAX      = 0
         MG_PERF(mype+1)%BCAST_COUNT_MAX     = 0
         MG_PERF(mype+1)%ALLREDUCE_COUNT_MAX = 0
   
         MG_PERF(mype+1)%SEND_COUNT_MIN      = 1000000000
         MG_PERF(mype+1)%RECV_COUNT_MIN      = 1000000000
         MG_PERF(mype+1)%BCAST_COUNT_MIN     = 1000000000
         MG_PERF(mype+1)%ALLREDUCE_COUNT_MIN = 1000000000

         ! Initialize timer variables.

         MG_PERF(mype+1)%TIME_WALL_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_PACK_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_BSPMA_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_SVAF_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_PACK_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_SEND_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_WAIT_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_RECV_PE    = 0.0D+0
         MG_PERF(mype+1)%TIME_UNPACK_PE  = 0.0D+0
         MG_PERF(mype+1)%TIME_STENCIL_PE = 0.0D+0

         MG_PERF(mype+1)%TIME_PACK_X_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_SEND_X_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_WAIT_X_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_RECV_X_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_UNPACK_X_PE = 0.0D+0

         MG_PERF(mype+1)%TIME_PACK_Y_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_SEND_Y_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_WAIT_Y_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_RECV_Y_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_UNPACK_Y_PE = 0.0D+0

         MG_PERF(mype+1)%TIME_PACK_Z_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_SEND_Z_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_WAIT_Z_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_RECV_Z_PE   = 0.0D+0
         MG_PERF(mype+1)%TIME_UNPACK_Z_PE = 0.0D+0

         MG_PERF(mype+1)%TIME_SVCP_PE      = 0.0D+0

         MG_PERF(mype+1)%TIME_SUMGRID_PE      = 0.0D+0
         MG_PERF(mype+1)%TIME_SUMGRID_COMP_PE = 0.0D+0
         MG_PERF(mype+1)%TIME_SUMGRID_COMM_PE = 0.0D+0

      END SUBROUTINE PERF_INIT

!     ==================================================================

      recursive SUBROUTINE PERF_REPORT ( COMM_METHOD, STENCIL, IERR, mype )

      IMPLICIT NONE

      ! Argument Declarations
      INTEGER(KIND=MG_INT), INTENT(IN)  ::       &
         COMM_METHOD, STENCIL

      INTEGER, INTENT(OUT) :: IERR, mype

      !  Purpose
      !  =======
      !  Collate, process, and report performance results.

      ! Local Scalars
      CHARACTER(LEN=30) ::        &
         TEST_DATE
      CHARACTER(LEN=30) ::        &
         TEST_TIME

      INTEGER, PARAMETER :: &
         INUM_ADDS        = 1,           &
         INUM_MULTS       = 2,           &
         INUM_COPYS       = 3,           &

         INUM_SENDS       = 4,           &
         ISEND_COUNT      = 5,           &
         INUM_RECVS       = 6,           &
         IRECV_COUNT      = 7,           &
 
         INUM_BCASTS      = 8,           &
         IBCAST_COUNT     = 9,           &
         INUM_ALLREDUCES  = 10,          &
         IALLREDUCE_COUNT = 11,          &

         INUM_SUMGRID     = 12,          &

         NUM_PARAMS       = 12

      INTEGER ::            &
         I,                              & ! Counter
         J,                              & ! Counter
         ICLOCK_RATE,                    &
         IDUM,                           &
         LEN,                            &
         NUM_TO_MAX,                     &
         NUM_TO_MIN,                     &
         NUM_TO_SUM,                     &
         OUTPUT_LOC


          

     ! Global values:

      INTEGER(KIND=MG_INT8) :: &
         GNUM_ADDS,                                        &
         GNUM_MULTS,                                       &
         GNUM_COPY,                                        &

         GNUM_SENDS,                                       &
         GSEND_COUNT,                                      &
         GSEND_COUNT_MAX,                                  &
         GSEND_COUNT_MIN,                                  &
         GSEND_NODE_NUM_MAX,                               &
         GSEND_NODE_NUM_MIN,                               &
         GSEND_NODE_COUNT_MAX,                             &
         GSEND_NODE_COUNT_MIN,                             &

         GNUM_RECVS,                                       &
         GRECV_COUNT,                                      &
         GRECV_COUNT_MAX,                                  &
         GRECV_COUNT_MIN,                                  &
         GRECV_NODE_NUM_MAX,                               &
         GRECV_NODE_NUM_MIN,                               &
         GRECV_NODE_COUNT_MAX,                             &
         GRECV_NODE_COUNT_MIN,                             &

         GBCAST_COUNT_MAX,                                 &
         GBCAST_COUNT_MIN,                                 &

         GALLREDUCE_COUNT_MAX,                             &
         GALLREDUCE_COUNT_MIN

      INTEGER(KIND=MG_INT8), DIMENSION(:), ALLOCATABLE ::   &
         MAX_THESE,                                         &
         MIN_THESE,                                         &
         SUM_THESE,                                         &
         THESE_MAXED,                                       &
         THESE_MINNED,                                      &
         THESE_SUMMED

      REAL(KIND=MG_REAL4) ::       &
         CLOCK_RES = 0.0

      REAL(KIND=MG_REAL8) ::       &
         GFLOPS,                   &

         TIME_COMM_METHOD_AVG,     &  ! Coarsest grain of communication requirements.
         TIME_COMM_METHOD_MAX,     &
         TIME_COMM_METHOD_MIN,     &
         TIME_COMM_METHOD_SUM,     &

         TIME_COMMSUM_AVG,         & ! Sum of individual comm routines, eg pack, send, recv, unpack. 
         TIME_COMMSUM_MAX,         &
         TIME_COMMSUM_MIN,         &
         TIME_COMMSUM_SUM,         &

         TIME_COMMSUM_X_AVG,       & ! For three directions
         TIME_COMMSUM_X_MAX,       &
         TIME_COMMSUM_X_MIN,       &
         TIME_COMMSUM_X_SUM,       &

         TIME_COMMSUM_Y_AVG,       &
         TIME_COMMSUM_Y_MAX,       &
         TIME_COMMSUM_Y_MIN,       &
         TIME_COMMSUM_Y_SUM,       &

         TIME_COMMSUM_Z_AVG,       &
         TIME_COMMSUM_Z_MAX,       &
         TIME_COMMSUM_Z_MIN,       &
         TIME_COMMSUM_Z_SUM,       &

         TIME_WALL_MAX,            & ! Total time around time stepping.
         TIME_WALL_MIN,            &
         TIME_WALL_AVG,            &
         TIME_WALL_SUM,            &

         TIME_PACK_AVG,            & ! Comm explicit data packing requirements.
         TIME_PACK_MAX,            &
         TIME_PACK_MIN,            &
         TIME_PACK_SUM,            &

         TIME_PACK_X_AVG,          &
         TIME_PACK_X_MAX,          &
         TIME_PACK_X_MIN,          &
         TIME_PACK_X_SUM,          &

         TIME_PACK_Y_AVG,          &
         TIME_PACK_Y_MAX,          &
         TIME_PACK_Y_MIN,          &
         TIME_PACK_Y_SUM,          &

         TIME_PACK_Z_AVG,          &
         TIME_PACK_Z_MAX,          &
         TIME_PACK_Z_MIN,          &
         TIME_PACK_Z_SUM,          &

         TIME_WAIT_AVG,            & ! Comm explicit data waiting requirements.
         TIME_WAIT_MAX,            &
         TIME_WAIT_MIN,            &
         TIME_WAIT_SUM,            &

         TIME_WAIT_X_AVG,          &
         TIME_WAIT_X_MAX,          &
         TIME_WAIT_X_MIN,          &
         TIME_WAIT_X_SUM,          &

         TIME_WAIT_Y_AVG,          &
         TIME_WAIT_Y_MAX,          &
         TIME_WAIT_Y_MIN,          &
         TIME_WAIT_Y_SUM,          &

         TIME_WAIT_Z_AVG,          &
         TIME_WAIT_Z_MAX,          &
         TIME_WAIT_Z_MIN,          &
         TIME_WAIT_Z_SUM,          &

         TIME_SEND_AVG,            & ! Comm explicit send requirements.
         TIME_SEND_MAX,            &
         TIME_SEND_MIN,            &
         TIME_SEND_SUM,            &

         TIME_SEND_X_AVG,          &
         TIME_SEND_X_MAX,          &
         TIME_SEND_X_MIN,          &
         TIME_SEND_X_SUM,          &

         TIME_SEND_Y_AVG,          &
         TIME_SEND_Y_MAX,          &
         TIME_SEND_Y_MIN,          &
         TIME_SEND_Y_SUM,          &

         TIME_SEND_Z_AVG,          &
         TIME_SEND_Z_MAX,          &
         TIME_SEND_Z_MIN,          &
         TIME_SEND_Z_SUM,          &

         TIME_RECV_AVG,            & ! Comm explicit recv requirements.
         TIME_RECV_MAX,            &
         TIME_RECV_MIN,            &
         TIME_RECV_SUM,            &

         TIME_RECV_X_AVG,          &
         TIME_RECV_X_MAX,          &
         TIME_RECV_X_MIN,          &
         TIME_RECV_X_SUM,          &

         TIME_RECV_Y_AVG,          &
         TIME_RECV_Y_MAX,          &
         TIME_RECV_Y_MIN,          &
         TIME_RECV_Y_SUM,          &

         TIME_RECV_Z_AVG,          &
         TIME_RECV_Z_MAX,          &
         TIME_RECV_Z_MIN,          &
         TIME_RECV_Z_SUM,          &

         TIME_UNPACK_AVG,          & ! Comm explicit data unpacking requirements.
         TIME_UNPACK_MAX,          &
         TIME_UNPACK_MIN,          &
         TIME_UNPACK_SUM,          &

         TIME_UNPACK_X_AVG,        &
         TIME_UNPACK_X_MAX,        &
         TIME_UNPACK_X_MIN,        &
         TIME_UNPACK_X_SUM,        &

         TIME_UNPACK_Y_AVG,        &
         TIME_UNPACK_Y_MAX,        &
         TIME_UNPACK_Y_MIN,        &
         TIME_UNPACK_Y_SUM,        &

         TIME_UNPACK_Z_AVG,        &
         TIME_UNPACK_Z_MAX,        &
         TIME_UNPACK_Z_MIN,        &
         TIME_UNPACK_Z_SUM,        &

         TIME_STENCIL_AVG,         & ! Computation, external of communication, requirements.
         TIME_STENCIL_MAX,         &
         TIME_STENCIL_MIN,         &
         TIME_STENCIL_SUM,         &

         TIME_SUMGRID_AVG,         & ! Grid summation time.
         TIME_SUMGRID_MAX,         &
         TIME_SUMGRID_MIN,         &
         TIME_SUMGRID_SUM,         &

         TIME_SUMGRID_COMP_AVG,    & ! Grid summation computation requirements.
         TIME_SUMGRID_COMP_MAX,    &
         TIME_SUMGRID_COMP_MIN,    &
         TIME_SUMGRID_COMP_SUM,    &

         TIME_SUMGRID_COMM_AVG,    & ! Grid summation communication requirements.
         TIME_SUMGRID_COMM_MAX,    &
         TIME_SUMGRID_COMM_MIN,    &
         TIME_SUMGRID_COMM_SUM,    &

         TIME_WALL_ALL(NUMPES),         &
         TIME_COMM_METHOD_ALL(NUMPES),  &
         TIME_COMM_ALL(NUMPES),         &
         TIME_PACK_ALL(NUMPES),         &
         TIME_WAIT_ALL(NUMPES),         &
         TIME_SEND_ALL(NUMPES),         &
         TIME_RECV_ALL(NUMPES),         &
         TIME_UNPACK_ALL(NUMPES),       &
         TIME_COMM_X_ALL(NUMPES),       &
         TIME_PACK_X_ALL(NUMPES),       &
         TIME_SEND_X_ALL(NUMPES),       &
         TIME_WAIT_X_ALL(NUMPES),       &
         TIME_RECV_X_ALL(NUMPES),       &
         TIME_UNPACK_X_ALL(NUMPES),     &
         TIME_COMM_Y_ALL(NUMPES),       &
         TIME_PACK_Y_ALL(NUMPES),       &
         TIME_SEND_Y_ALL(NUMPES),       &
         TIME_WAIT_Y_ALL(NUMPES),       &
         TIME_RECV_Y_ALL(NUMPES),       &
         TIME_UNPACK_Y_ALL(NUMPES),     &
         TIME_COMM_Z_ALL(NUMPES),       &
         TIME_PACK_Z_ALL(NUMPES),       &
         TIME_SEND_Z_ALL(NUMPES),       &
         TIME_WAIT_Z_ALL(NUMPES),       &
         TIME_RECV_Z_ALL(NUMPES),       &
         TIME_UNPACK_Z_ALL(NUMPES),     &
         TIME_STENCIL_ALL(NUMPES),      &
         TIME_SVCP_ALL(NUMPES),      &
         TIME_SUMGRID_ALL(NUMPES),      &
         TIME_SUMGRID_COMP_ALL(NUMPES), &
         TIME_SUMGRID_COMM_ALL(NUMPES)

      INTEGER, PARAMETER ::  &
         SIZE_OF_DATA = 8

      REAL(KIND=MG_REAL8), PARAMETER ::    &
         GIGA = 1000000000.0D+0

	  integer :: world
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      world = MPI_COMM_WORLD



      ! Gather timings to root process.
	write(*,*) 'rank ',mype,' reporting my time: ',MG_PERF(mype+1)%TIME_WALL_PE
      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_WALL_PE, 1, MPI_REAL8,           &
                        TIME_WALL_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
       IF ( linfo(mype+1)%MYPE == ROOT_PE ) THEN
       DO I = 1, NUMPES

       	write(*,*) 'time at index ',I,' is ',TIME_WALL_ALL(I)
       END DO

       END IF
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_WALL_PE)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_PACK_PE, 1, MPI_REAL8,           &
                        TIME_PACK_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_PACK)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SEND_PE, 1, MPI_REAL8,           &
                        TIME_SEND_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SEND)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_WAIT_PE, 1, MPI_REAL8,           &
                        TIME_WAIT_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_WAIT)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_RECV_PE, 1, MPI_REAL8,           &
                        TIME_RECV_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_RECV)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_UNPACK_PE, 1, MPI_REAL8,         &
                        TIME_UNPACK_ALL,        1, MPI_REAL8,         &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_UNPACK)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_PACK_X_PE, 1, MPI_REAL8,           &
                        TIME_PACK_X_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_PACK_X)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SEND_X_PE, 1, MPI_REAL8,           &
                        TIME_SEND_X_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SEND_X)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_WAIT_X_PE, 1, MPI_REAL8,           &
                        TIME_WAIT_X_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_WAIT_X)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_RECV_X_PE, 1, MPI_REAL8,           &
                        TIME_RECV_X_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_RECV_X)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_UNPACK_X_PE, 1, MPI_REAL8,         &
                        TIME_UNPACK_X_ALL,        1, MPI_REAL8,         &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_UNPACK_X)', 1, mype)

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_PACK_Y_PE, 1, MPI_REAL8,           &
                        TIME_PACK_Y_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_PACK_Y)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SEND_Y_PE, 1, MPI_REAL8,           &
                        TIME_SEND_Y_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SEND_Y)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_WAIT_Y_PE, 1, MPI_REAL8,           &
                        TIME_WAIT_Y_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_WAIT_Y)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_RECV_Y_PE, 1, MPI_REAL8,           &
                        TIME_RECV_Y_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_RECV_Y)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_UNPACK_Y_PE, 1, MPI_REAL8,         &
                        TIME_UNPACK_Y_ALL,        1, MPI_REAL8,         &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_UNPACK_Y)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_PACK_Z_PE, 1, MPI_REAL8,           &
                        TIME_PACK_Z_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_PACK_Z)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SEND_Z_PE, 1, MPI_REAL8,           &
                        TIME_SEND_Z_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SEND_Z)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_WAIT_Z_PE, 1, MPI_REAL8,           &
                        TIME_WAIT_Z_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_WAIT_Z)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_RECV_Z_PE, 1, MPI_REAL8,           &
                        TIME_RECV_Z_ALL,        1, MPI_REAL8,           &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_RECV_Z)', 1, mype )

      CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_UNPACK_Z_PE, 1, MPI_REAL8,         &
                        TIME_UNPACK_Z_ALL,        1, MPI_REAL8,         &
                        ROOT_PE, world, IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_UNPACK_Z)', 1, mype )

      SELECT CASE ( COMM_METHOD )

         CASE ( COMM_METHOD_BSPMA )

            CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_BSPMA_PE, 1, MPI_REAL8,           &
                              TIME_COMM_METHOD_ALL,  1, MPI_REAL8,           &
                              ROOT_PE, world, IERR )
            CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_BSPMA)', 1, mype )

         CASE ( COMM_METHOD_SVAF )

            CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SVAF_PE,  1, MPI_REAL8,           &
                              TIME_COMM_METHOD_ALL,  1, MPI_REAL8,           &
                              ROOT_PE, world, IERR )
            CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SVAF)', 1, mype )

      END SELECT

      IF ( &
           ( STENCIL == STENCIL_2D5PT ) .OR.   &
           ( STENCIL == STENCIL_2D9PT ) .OR.   &
           ( STENCIL == STENCIL_3D7PT ) .OR.   &
           ( STENCIL == STENCIL_3D27PT ) )     &
         THEN

            CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_STENCIL_PE,  1, MPI_REAL8,           &
                              TIME_STENCIL_ALL,         1, MPI_REAL8,           &
                              ROOT_PE, world, IERR )
            CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_STENCIL)', 1, mype )

      END IF

      IF ( COMM_METHOD == COMM_METHOD_SVCP ) THEN

         CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SVCP_PE, 1, MPI_REAL8,           &
                           TIME_SVCP_ALL,        1, MPI_REAL8,           &
                           ROOT_PE, world, IERR )
         CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SVCP)', 1, mype )

      END IF

      IF ( MG_PERF(mype+1)%NUM_SUMGRID /= 0) THEN
         CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SUMGRID_PE,  1, MPI_REAL8,           &
                           TIME_SUMGRID_ALL,         1, MPI_REAL8,           &
                           ROOT_PE, world, IERR )
         CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_ALLRED)', 1, mype )

         CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SUMGRID_COMP_PE,  1, MPI_REAL8,       &
                           TIME_SUMGRID_COMP_ALL,         1, MPI_REAL8,       &
                           ROOT_PE, world, IERR )
         CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SUMGRID_COM)', 1, mype )

         CALL MPI_GATHER ( MG_PERF(mype+1)%TIME_SUMGRID_COMM_PE,  1, MPI_REAL8,       &
                           TIME_SUMGRID_COMM_ALL,         1, MPI_REAL8,       &
                           ROOT_PE, world, IERR )
         CALL CHECK_ERROR ( IERR, 'REPORT_PERF: MPI_GATHER(TIME_SUMGRID_RED)', 1, mype )
      END IF

      ! Process COUNTS.

      ALLOCATE ( MAX_THESE(NUM_PARAMS),    STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: ALLOCATE ( MAX_THESE', NUM_PARAMS, mype )

      ALLOCATE ( MIN_THESE(NUM_PARAMS),    STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: ALLOCATE ( MIN_THESE', NUM_PARAMS, mype )

      ! Note that don't sum collectives, but do max/min their counts.

      ALLOCATE ( SUM_THESE(NUM_PARAMS),    STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: ALLOCATE ( SUM_THESE', NUM_PARAMS, mype )

      ALLOCATE ( THESE_MAXED(NUM_PARAMS),  STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: ALLOCATE ( THESE_MAXED', NUM_PARAMS, mype )

      ALLOCATE ( THESE_MINNED(NUM_PARAMS), STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: ALLOCATE ( THESE_MINNED', NUM_PARAMS, mype )

      ALLOCATE ( THESE_SUMMED(NUM_PARAMS), STAT=IERR )
      CALL CHECK_ERROR ( IERR, 'REPORT_PERF: ALLOCATE ( THESE_SUMMED', NUM_PARAMS, mype )

      NUM_TO_SUM = 1
      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%NUM_ADDS
      NUM_TO_SUM = NUM_TO_SUM + 1
      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%NUM_MULTS
      NUM_TO_SUM = NUM_TO_SUM + 1
      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%NUM_COPY
      NUM_TO_SUM = NUM_TO_SUM + 1

      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%NUM_SENDS
      NUM_TO_SUM = NUM_TO_SUM + 1
      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%SEND_COUNT
      NUM_TO_SUM = NUM_TO_SUM + 1
      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%NUM_RECVS
      NUM_TO_SUM = NUM_TO_SUM + 1
      SUM_THESE(NUM_TO_SUM) = MG_PERF(mype+1)%RECV_COUNT
   
      NUM_TO_MAX = 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%SEND_COUNT_MAX
      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%RECV_COUNT_MAX
      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%BCAST_COUNT_MAX
      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%ALLREDUCE_COUNT_MAX

      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%NUM_SENDS
      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%SEND_COUNT
      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%NUM_RECVS
      NUM_TO_MAX = NUM_TO_MAX + 1
      MAX_THESE(NUM_TO_MAX) = MG_PERF(mype+1)%RECV_COUNT

      NUM_TO_MIN = 1
      IF ( MG_PERF(mype+1)%NUM_SENDS /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%SEND_COUNT_MIN
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
      IF ( MG_PERF(mype+1)%NUM_RECVS /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%RECV_COUNT_MIN
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
      IF ( MG_PERF(mype+1)%NUM_BCASTS /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%BCAST_COUNT_MIN
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
      IF ( MG_PERF(mype+1)%NUM_ALLREDUCES /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%ALLREDUCE_COUNT_MIN
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1

      IF ( MG_PERF(mype+1)%NUM_SENDS /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%NUM_SENDS
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
      IF ( MG_PERF(mype+1)%SEND_COUNT /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%SEND_COUNT
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
      IF ( MG_PERF(mype+1)%NUM_RECVS /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%NUM_RECVS
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
      IF ( MG_PERF(mype+1)%RECV_COUNT /= 0 ) THEN
         MIN_THESE(NUM_TO_MIN) = MG_PERF(mype+1)%RECV_COUNT
      ELSE
         MIN_THESE(NUM_TO_MIN) = 0
      END IF
      NUM_TO_MIN = NUM_TO_MIN + 1
 
      CALL MPI_REDUCE ( SUM_THESE, THESE_SUMMED, NUM_TO_SUM, MPI_INTEGER8, &
                        MPI_SUM, ROOT_PE, world, IERR )
      IF ( IERR /= MPI_SUCCESS ) THEN
         WRITE(*,*) '** Error ** PERF_REPORT: MPI_REDUCE(SUM_THESE); IERR = ', IERR
         IERR = -1
         RETURN
      END IF
      CALL MPI_REDUCE ( MAX_THESE, THESE_MAXED, NUM_TO_MAX, MPI_INTEGER8, &
                        MPI_MAX, ROOT_PE, world, IERR )
      IF ( IERR /= MPI_SUCCESS ) THEN
         WRITE(*,*) '** Error ** PERF_REPORT: MPI_REDUCE(MAX_THESE); IERR = ', IERR
         IERR = -1
         RETURN
      END IF
      CALL MPI_REDUCE ( MIN_THESE, THESE_MINNED, NUM_TO_MIN, MPI_INTEGER8,  &
                        MPI_MIN, ROOT_PE, world, IERR )
      IF ( IERR /= MPI_SUCCESS ) THEN
         WRITE(*,*) '** Error ** PERF_REPORT: MPI_REDUCE(MIN_THESE); IERR = ', IERR
         IERR = -1
         RETURN
      END IF

      IF ( linfo(mype+1)%MYPE /= ROOT_PE ) &
         RETURN

      NUM_TO_SUM  = 1
      GNUM_ADDS   = THESE_SUMMED(NUM_TO_SUM)
      NUM_TO_SUM  = NUM_TO_SUM + 1
      GNUM_MULTS  = THESE_SUMMED(NUM_TO_SUM)
      NUM_TO_SUM  = NUM_TO_SUM + 1
      GNUM_MULTS  = THESE_SUMMED(NUM_TO_SUM)
      NUM_TO_SUM  = NUM_TO_SUM + 1
      GNUM_SENDS  = THESE_SUMMED(NUM_TO_SUM)
      NUM_TO_SUM  = NUM_TO_SUM + 1
      GSEND_COUNT = THESE_SUMMED(NUM_TO_SUM)
      NUM_TO_SUM  = NUM_TO_SUM + 1
      GNUM_RECVS  = THESE_SUMMED(NUM_TO_SUM)
      NUM_TO_SUM  = NUM_TO_SUM + 1
      GRECV_COUNT = THESE_SUMMED(NUM_TO_SUM)

      NUM_TO_MAX = 1
      GSEND_COUNT_MAX      = THESE_MAXED(NUM_TO_MAX)
      NUM_TO_MAX = NUM_TO_MAX + 1
      GRECV_COUNT_MAX      = THESE_MAXED(NUM_TO_MAX)
      NUM_TO_MAX = NUM_TO_MAX + 1
      GBCAST_COUNT_MAX     = THESE_MAXED(NUM_TO_MAX)
      NUM_TO_MAX = NUM_TO_MAX + 1
      GALLREDUCE_COUNT_MAX = THESE_MAXED(NUM_TO_MAX)

      NUM_TO_MAX = NUM_TO_MAX + 1
      GSEND_NODE_NUM_MAX   = THESE_MAXED(NUM_TO_MAX)
      NUM_TO_MAX = NUM_TO_MAX + 1
      GSEND_NODE_COUNT_MAX = THESE_MAXED(NUM_TO_MAX)
      NUM_TO_MAX = NUM_TO_MAX + 1
      GRECV_NODE_NUM_MAX   = THESE_MAXED(NUM_TO_MAX)
      NUM_TO_MAX = NUM_TO_MAX + 1
      GRECV_NODE_COUNT_MAX = THESE_MAXED(NUM_TO_MAX)

      NUM_TO_MIN = 1
      GSEND_COUNT_MIN      = THESE_MINNED(NUM_TO_MIN)
      NUM_TO_MIN = NUM_TO_MIN + 1
      GRECV_COUNT_MIN      = THESE_MINNED(NUM_TO_MIN)
      NUM_TO_MIN = NUM_TO_MIN + 1
      GBCAST_COUNT_MIN     = THESE_MINNED(NUM_TO_MIN)
      NUM_TO_MIN = NUM_TO_MIN + 1
      GALLREDUCE_COUNT_MIN = THESE_MINNED(NUM_TO_MIN)

      NUM_TO_MIN = NUM_TO_MIN + 1
      GSEND_NODE_NUM_MIN   = THESE_MINNED(NUM_TO_MIN)
      NUM_TO_MIN = NUM_TO_MIN + 1
      GSEND_NODE_COUNT_MIN = THESE_MINNED(NUM_TO_MIN)
      NUM_TO_MIN = NUM_TO_MIN + 1
      GRECV_NODE_NUM_MIN   = THESE_MINNED(NUM_TO_MIN)
      NUM_TO_MIN = NUM_TO_MIN + 1
      GRECV_NODE_COUNT_MIN = THESE_MINNED(NUM_TO_MIN)

      ! Root process writes out global data:
      TIME_WALL_MIN = TIME_WALL_ALL( 1 )
      TIME_WALL_MAX = TIME_WALL_ALL( 1 )
      TIME_WALL_SUM = 0.0

      DO I = 2, NUMPES
         TIME_WALL_MIN = MIN( TIME_WALL_MIN, TIME_WALL_ALL( I ) )
         TIME_WALL_MAX = MAX( TIME_WALL_MAX, TIME_WALL_ALL( I ) )
         TIME_WALL_SUM = TIME_WALL_SUM + TIME_WALL_ALL( I )
      END DO
      TIME_WALL_AVG = TIME_WALL_SUM / NUMPES

      TIME_COMM_METHOD_MIN = TIME_COMM_METHOD_ALL( 1 )
      TIME_COMM_METHOD_MAX = TIME_COMM_METHOD_ALL( 1 )
      TIME_COMM_METHOD_SUM = 0.0

      DO I = 2, NUMPES
         TIME_COMM_METHOD_MIN = MIN( TIME_COMM_METHOD_MIN, TIME_COMM_METHOD_ALL( I ) )
         TIME_COMM_METHOD_MAX = MAX( TIME_COMM_METHOD_MAX, TIME_COMM_METHOD_ALL( I ) )
         TIME_COMM_METHOD_SUM = TIME_COMM_METHOD_SUM + TIME_COMM_METHOD_ALL( I )
      END DO
      TIME_COMM_METHOD_AVG = TIME_COMM_METHOD_SUM / NUMPES


      IF ( STENCIL /= STENCIL_NONE ) THEN
         TIME_STENCIL_MIN = TIME_STENCIL_ALL( 1 )
         TIME_STENCIL_MAX = TIME_STENCIL_ALL( 1 )
         TIME_STENCIL_SUM = TIME_STENCIL_ALL( 1 )
   
         DO I = 2, NUMPES
            TIME_STENCIL_MIN = MIN( TIME_STENCIL_MIN, TIME_STENCIL_ALL( I ) )
            TIME_STENCIL_MAX = MAX( TIME_STENCIL_MAX, TIME_STENCIL_ALL( I ) )
            TIME_STENCIL_SUM  = TIME_STENCIL_SUM + TIME_STENCIL_ALL( I )
         END DO
      END IF
      TIME_STENCIL_AVG = TIME_STENCIL_SUM / NUMPES

      DO I = 1, NUMPES
         TIME_COMM_ALL(I) = TIME_PACK_ALL(I) + &
                            TIME_SEND_ALL(I) + &
                            TIME_WAIT_ALL(I) + &
                            TIME_RECV_ALL(I) + &
                            TIME_UNPACK_ALL(I)
         TIME_COMM_X_ALL(I) = TIME_PACK_X_ALL(I) + &
                              TIME_SEND_X_ALL(I) + &
                              TIME_WAIT_X_ALL(I) + &
                              TIME_RECV_X_ALL(I) + &
                              TIME_UNPACK_X_ALL(I)
         TIME_COMM_Y_ALL(I) = TIME_PACK_Y_ALL(I) + &
                              TIME_SEND_Y_ALL(I) + &
                              TIME_WAIT_Y_ALL(I) + &
                              TIME_RECV_Y_ALL(I) + &
                              TIME_UNPACK_Y_ALL(I)
         TIME_COMM_Z_ALL(I) = TIME_PACK_Z_ALL(I) + &
                              TIME_SEND_Z_ALL(I) + &
                              TIME_WAIT_Z_ALL(I) + &
                              TIME_RECV_Z_ALL(I) + &
                              TIME_UNPACK_Z_ALL(I)
      END DO

      TIME_COMMSUM_SUM = TIME_COMM_ALL( 1 )
      TIME_COMMSUM_MAX = TIME_COMM_ALL( 1 )
      TIME_COMMSUM_MIN = TIME_COMM_ALL( 1 )
      DO I = 2, NUMPES
         TIME_COMMSUM_MIN = MIN( TIME_COMMSUM_MIN, TIME_COMM_ALL( I ) )
         TIME_COMMSUM_MAX = MAX( TIME_COMMSUM_MAX, TIME_COMM_ALL( I ) )
         TIME_COMMSUM_SUM = TIME_COMMSUM_SUM + TIME_COMM_ALL( I )
      END DO
      TIME_COMMSUM_AVG = TIME_COMMSUM_SUM / NUMPES

      TIME_PACK_SUM = TIME_PACK_ALL( 1 )
      TIME_PACK_MAX = TIME_PACK_ALL( 1 )
      TIME_PACK_MIN = TIME_PACK_ALL( 1 )
      DO I = 2, NUMPES
         TIME_PACK_MIN = MIN( TIME_PACK_MIN, TIME_PACK_ALL( I ) )
         TIME_PACK_MAX = MAX( TIME_PACK_MAX, TIME_PACK_ALL( I ) )
         TIME_PACK_SUM = TIME_PACK_SUM + TIME_PACK_ALL( I )
      END DO
      TIME_PACK_AVG = TIME_PACK_SUM / NUMPES

      TIME_SEND_SUM = TIME_SEND_ALL( 1 )
      TIME_SEND_MAX = TIME_SEND_ALL( 1 )
      TIME_SEND_MIN = TIME_SEND_ALL( 1 )
      DO I = 2, NUMPES
         TIME_SEND_MIN = MIN( TIME_SEND_MIN, TIME_SEND_ALL( I ) )
         TIME_SEND_MAX = MAX( TIME_SEND_MAX, TIME_SEND_ALL( I ) )
         TIME_SEND_SUM = TIME_SEND_SUM + TIME_SEND_ALL( I )
      END DO
      TIME_SEND_AVG = TIME_SEND_SUM / NUMPES

      TIME_WAIT_SUM = TIME_WAIT_ALL( 1 )
      TIME_WAIT_MAX = TIME_WAIT_ALL( 1 )
      TIME_WAIT_MIN = TIME_WAIT_ALL( 1 )
      DO I = 2, NUMPES
         TIME_WAIT_MIN = MIN( TIME_WAIT_MIN, TIME_WAIT_ALL( I ) )
         TIME_WAIT_MAX = MAX( TIME_WAIT_MAX, TIME_WAIT_ALL( I ) )
         TIME_WAIT_SUM = TIME_WAIT_SUM + TIME_WAIT_ALL( I )
      END DO
      TIME_WAIT_AVG = TIME_WAIT_SUM / NUMPES

      TIME_RECV_SUM = TIME_RECV_ALL( 1 )
      TIME_RECV_MAX = TIME_RECV_ALL( 1 )
      TIME_RECV_MIN = TIME_RECV_ALL( 1 )
      DO I = 2, NUMPES
         TIME_RECV_MIN = MIN( TIME_RECV_MIN, TIME_RECV_ALL( I ) )
         TIME_RECV_MAX = MAX( TIME_RECV_MAX, TIME_RECV_ALL( I ) )
         TIME_RECV_SUM = TIME_RECV_SUM + TIME_RECV_ALL( I )
      END DO
      TIME_RECV_AVG = TIME_RECV_SUM / NUMPES

      TIME_UNPACK_SUM = TIME_UNPACK_ALL( 1 )
      TIME_UNPACK_MAX = TIME_UNPACK_ALL( 1 )
      TIME_UNPACK_MIN = TIME_UNPACK_ALL( 1 )
      DO I = 2, NUMPES
         TIME_UNPACK_MIN = MIN( TIME_UNPACK_MIN, TIME_UNPACK_ALL( I ) )
         TIME_UNPACK_MAX = MAX( TIME_UNPACK_MAX, TIME_UNPACK_ALL( I ) )
         TIME_UNPACK_SUM = TIME_UNPACK_SUM + TIME_UNPACK_ALL( I )
      END DO
      TIME_UNPACK_AVG = TIME_UNPACK_SUM / NUMPES

      TIME_COMMSUM_X_SUM = TIME_COMM_X_ALL( 1 )
      TIME_COMMSUM_X_MAX = TIME_COMM_X_ALL( 1 )
      TIME_COMMSUM_X_MIN = TIME_COMM_X_ALL( 1 )
      DO I = 2, NUMPES
         TIME_COMMSUM_X_MIN = MIN( TIME_COMMSUM_X_MIN, TIME_COMM_X_ALL( I ) )
         TIME_COMMSUM_X_MAX = MAX( TIME_COMMSUM_X_MAX, TIME_COMM_X_ALL( I ) )
         TIME_COMMSUM_X_SUM = TIME_COMMSUM_X_SUM + TIME_COMM_X_ALL( I )
      END DO
      TIME_COMMSUM_X_AVG = TIME_COMMSUM_X_SUM / NUMPES

      TIME_PACK_X_SUM = TIME_PACK_X_ALL( 1 )
      TIME_PACK_X_MAX = TIME_PACK_X_ALL( 1 )
      TIME_PACK_X_MIN = TIME_PACK_X_ALL( 1 )
      DO I = 2, NUMPES
         TIME_PACK_X_MIN = MIN( TIME_PACK_X_MIN, TIME_PACK_X_ALL( I ) )
         TIME_PACK_X_MAX = MAX( TIME_PACK_X_MAX, TIME_PACK_X_ALL( I ) )
         TIME_PACK_X_SUM = TIME_PACK_X_SUM + TIME_PACK_X_ALL( I )
      END DO
      TIME_PACK_X_AVG = TIME_PACK_X_SUM / NUMPES

      TIME_SEND_X_SUM = TIME_SEND_X_ALL( 1 )
      TIME_SEND_X_MAX = TIME_SEND_X_ALL( 1 )
      TIME_SEND_X_MIN = TIME_SEND_X_ALL( 1 )
      DO I = 2, NUMPES
         TIME_SEND_X_MIN = MIN( TIME_SEND_X_MIN, TIME_SEND_X_ALL( I ) )
         TIME_SEND_X_MAX = MAX( TIME_SEND_X_MAX, TIME_SEND_X_ALL( I ) )
         TIME_SEND_X_SUM = TIME_SEND_X_SUM + TIME_SEND_X_ALL( I )
      END DO
      TIME_SEND_X_AVG = TIME_SEND_X_SUM / NUMPES

      TIME_WAIT_X_SUM = TIME_WAIT_X_ALL( 1 )
      TIME_WAIT_X_MAX = TIME_WAIT_X_ALL( 1 )
      TIME_WAIT_X_MIN = TIME_WAIT_X_ALL( 1 )
      DO I = 2, NUMPES
         TIME_WAIT_X_MIN = MIN( TIME_WAIT_X_MIN, TIME_WAIT_X_ALL( I ) )
         TIME_WAIT_X_MAX = MAX( TIME_WAIT_X_MAX, TIME_WAIT_X_ALL( I ) )
         TIME_WAIT_X_SUM = TIME_WAIT_X_SUM + TIME_WAIT_X_ALL( I )
      END DO
      TIME_WAIT_X_AVG = TIME_WAIT_X_SUM / NUMPES

      TIME_RECV_X_SUM = TIME_RECV_X_ALL( 1 )
      TIME_RECV_X_MAX = TIME_RECV_X_ALL( 1 )
      TIME_RECV_X_MIN = TIME_RECV_X_ALL( 1 )
      DO I = 2, NUMPES
         TIME_RECV_X_MIN = MIN( TIME_RECV_X_MIN, TIME_RECV_X_ALL( I ) )
         TIME_RECV_X_MAX = MAX( TIME_RECV_X_MAX, TIME_RECV_X_ALL( I ) )
         TIME_RECV_X_SUM = TIME_RECV_X_SUM + TIME_RECV_X_ALL( I )
      END DO
      TIME_RECV_X_AVG = TIME_RECV_X_SUM / NUMPES

      TIME_UNPACK_X_SUM = TIME_UNPACK_X_ALL( 1 )
      TIME_UNPACK_X_MAX = TIME_UNPACK_X_ALL( 1 )
      TIME_UNPACK_X_MIN = TIME_UNPACK_X_ALL( 1 )
      DO I = 2, NUMPES
         TIME_UNPACK_X_MIN = MIN( TIME_UNPACK_X_MIN, TIME_UNPACK_X_ALL( I ) )
         TIME_UNPACK_X_MAX = MAX( TIME_UNPACK_X_MAX, TIME_UNPACK_X_ALL( I ) )
         TIME_UNPACK_X_SUM = TIME_UNPACK_X_SUM + TIME_UNPACK_X_ALL( I )
      END DO
      TIME_UNPACK_X_AVG = TIME_UNPACK_X_SUM / NUMPES

      TIME_COMMSUM_Y_SUM = TIME_COMM_Y_ALL( 1 )
      TIME_COMMSUM_Y_MAX = TIME_COMM_Y_ALL( 1 )
      TIME_COMMSUM_Y_MIN = TIME_COMM_Y_ALL( 1 )
      DO I = 2, NUMPES
         TIME_COMMSUM_Y_MIN = MIN( TIME_COMMSUM_Y_MIN, TIME_COMM_Y_ALL( I ) )
         TIME_COMMSUM_Y_MAX = MAX( TIME_COMMSUM_Y_MAX, TIME_COMM_Y_ALL( I ) )
         TIME_COMMSUM_Y_SUM = TIME_COMMSUM_Y_SUM + TIME_COMM_Y_ALL( I )
      END DO
      TIME_COMMSUM_Y_AVG = TIME_COMMSUM_Y_SUM / NUMPES

      TIME_PACK_Y_SUM = TIME_PACK_Y_ALL( 1 )
      TIME_PACK_Y_MAX = TIME_PACK_Y_ALL( 1 )
      TIME_PACK_Y_MIN = TIME_PACK_Y_ALL( 1 )
      DO I = 2, NUMPES
         TIME_PACK_Y_MIN = MIN( TIME_PACK_Y_MIN, TIME_PACK_Y_ALL( I ) )
         TIME_PACK_Y_MAX = MAX( TIME_PACK_Y_MAX, TIME_PACK_Y_ALL( I ) )
         TIME_PACK_Y_SUM = TIME_PACK_Y_SUM + TIME_PACK_Y_ALL( I )
      END DO
      TIME_PACK_Y_AVG = TIME_PACK_Y_SUM / NUMPES

      TIME_SEND_Y_SUM = TIME_SEND_Y_ALL( 1 )
      TIME_SEND_Y_MAX = TIME_SEND_Y_ALL( 1 )
      TIME_SEND_Y_MIN = TIME_SEND_Y_ALL( 1 )
      DO I = 2, NUMPES
         TIME_SEND_Y_MIN = MIN( TIME_SEND_Y_MIN, TIME_SEND_Y_ALL( I ) )
         TIME_SEND_Y_MAX = MAX( TIME_SEND_Y_MAX, TIME_SEND_Y_ALL( I ) )
         TIME_SEND_Y_SUM = TIME_SEND_Y_SUM + TIME_SEND_Y_ALL( I )
      END DO
      TIME_SEND_Y_AVG = TIME_SEND_Y_SUM / NUMPES

      TIME_WAIT_Y_SUM = TIME_WAIT_Y_ALL( 1 )
      TIME_WAIT_Y_MAX = TIME_WAIT_Y_ALL( 1 )
      TIME_WAIT_Y_MIN = TIME_WAIT_Y_ALL( 1 )
      DO I = 2, NUMPES
         TIME_WAIT_Y_MIN = MIN( TIME_WAIT_Y_MIN, TIME_WAIT_Y_ALL( I ) )
         TIME_WAIT_Y_MAX = MAX( TIME_WAIT_Y_MAX, TIME_WAIT_Y_ALL( I ) )
         TIME_WAIT_Y_SUM = TIME_WAIT_Y_SUM + TIME_WAIT_Y_ALL( I )
      END DO
      TIME_WAIT_Y_AVG = TIME_WAIT_Y_SUM / NUMPES

      TIME_RECV_Y_SUM = TIME_RECV_Y_ALL( 1 )
      TIME_RECV_Y_MAX = TIME_RECV_Y_ALL( 1 )
      TIME_RECV_Y_MIN = TIME_RECV_Y_ALL( 1 )
      DO I = 2, NUMPES
         TIME_RECV_Y_MIN = MIN( TIME_RECV_Y_MIN, TIME_RECV_Y_ALL( I ) )
         TIME_RECV_Y_MAX = MAX( TIME_RECV_Y_MAX, TIME_RECV_Y_ALL( I ) )
         TIME_RECV_Y_SUM = TIME_RECV_Y_SUM + TIME_RECV_Y_ALL( I )
      END DO
      TIME_RECV_Y_AVG = TIME_RECV_Y_SUM / NUMPES

      TIME_UNPACK_Y_SUM = TIME_UNPACK_Y_ALL( 1 )
      TIME_UNPACK_Y_MAX = TIME_UNPACK_Y_ALL( 1 )
      TIME_UNPACK_Y_MIN = TIME_UNPACK_Y_ALL( 1 )
      DO I = 2, NUMPES
         TIME_UNPACK_Y_MIN = MIN( TIME_UNPACK_Y_MIN, TIME_UNPACK_Y_ALL( I ) )
         TIME_UNPACK_Y_MAX = MAX( TIME_UNPACK_Y_MAX, TIME_UNPACK_Y_ALL( I ) )
         TIME_UNPACK_Y_SUM = TIME_UNPACK_Y_SUM + TIME_UNPACK_Y_ALL( I )
      END DO
      TIME_UNPACK_Y_AVG = TIME_UNPACK_Y_SUM / NUMPES

      TIME_COMMSUM_Z_SUM = TIME_COMM_Z_ALL( 1 )
      TIME_COMMSUM_Z_MAX = TIME_COMM_Z_ALL( 1 )
      TIME_COMMSUM_Z_MIN = TIME_COMM_Z_ALL( 1 )
      DO I = 2, NUMPES
         TIME_COMMSUM_Z_MIN = MIN( TIME_COMMSUM_Z_MIN, TIME_COMM_Z_ALL( I ) )
         TIME_COMMSUM_Z_MAX = MAX( TIME_COMMSUM_Z_MAX, TIME_COMM_Z_ALL( I ) )
         TIME_COMMSUM_Z_SUM = TIME_COMMSUM_Z_SUM + TIME_COMM_Z_ALL( I )
      END DO
      TIME_COMMSUM_Z_AVG = TIME_COMMSUM_Z_SUM / NUMPES

      TIME_PACK_Z_SUM = TIME_PACK_Z_ALL( 1 )
      TIME_PACK_Z_MAX = TIME_PACK_Z_ALL( 1 )
      TIME_PACK_Z_MIN = TIME_PACK_Z_ALL( 1 )
      DO I = 2, NUMPES
         TIME_PACK_Z_MIN = MIN( TIME_PACK_Z_MIN, TIME_PACK_Z_ALL( I ) )
         TIME_PACK_Z_MAX = MAX( TIME_PACK_Z_MAX, TIME_PACK_Z_ALL( I ) )
         TIME_PACK_Z_SUM = TIME_PACK_Z_SUM + TIME_PACK_Z_ALL( I )
      END DO
      TIME_PACK_Z_AVG = TIME_PACK_Z_SUM / NUMPES

      TIME_SEND_Z_SUM = TIME_SEND_Z_ALL( 1 )
      TIME_SEND_Z_MAX = TIME_SEND_Z_ALL( 1 )
      TIME_SEND_Z_MIN = TIME_SEND_Z_ALL( 1 )
      DO I = 2, NUMPES
         TIME_SEND_Z_MIN = MIN( TIME_SEND_Z_MIN, TIME_SEND_Z_ALL( I ) )
         TIME_SEND_Z_MAX = MAX( TIME_SEND_Z_MAX, TIME_SEND_Z_ALL( I ) )
         TIME_SEND_Z_SUM = TIME_SEND_Z_SUM + TIME_SEND_Z_ALL( I )
      END DO
      TIME_SEND_Z_AVG = TIME_SEND_Z_SUM / NUMPES

      TIME_WAIT_Z_SUM = TIME_WAIT_Z_ALL( 1 )
      TIME_WAIT_Z_MAX = TIME_WAIT_Z_ALL( 1 )
      TIME_WAIT_Z_MIN = TIME_WAIT_Z_ALL( 1 )
      DO I = 2, NUMPES
         TIME_WAIT_Z_MIN = MIN( TIME_WAIT_Z_MIN, TIME_WAIT_Z_ALL( I ) )
         TIME_WAIT_Z_MAX = MAX( TIME_WAIT_Z_MAX, TIME_WAIT_Z_ALL( I ) )
         TIME_WAIT_Z_SUM = TIME_WAIT_Z_SUM + TIME_WAIT_Z_ALL( I )
      END DO
      TIME_WAIT_Z_AVG = TIME_WAIT_Z_SUM / NUMPES

      TIME_RECV_Z_SUM = TIME_RECV_Z_ALL( 1 )
      TIME_RECV_Z_MAX = TIME_RECV_Z_ALL( 1 )
      TIME_RECV_Z_MIN = TIME_RECV_Z_ALL( 1 )
      DO I = 2, NUMPES
         TIME_RECV_Z_MIN = MIN( TIME_RECV_Z_MIN, TIME_RECV_Z_ALL( I ) )
         TIME_RECV_Z_MAX = MAX( TIME_RECV_Z_MAX, TIME_RECV_Z_ALL( I ) )
         TIME_RECV_Z_SUM = TIME_RECV_Z_SUM + TIME_RECV_Z_ALL( I )
      END DO
      TIME_RECV_Z_AVG = TIME_RECV_Z_SUM / NUMPES

      TIME_UNPACK_Z_SUM = TIME_UNPACK_Z_ALL( 1 )
      TIME_UNPACK_Z_MAX = TIME_UNPACK_Z_ALL( 1 )
      TIME_UNPACK_Z_MIN = TIME_UNPACK_Z_ALL( 1 )
      DO I = 2, NUMPES
         TIME_UNPACK_Z_MIN = MIN( TIME_UNPACK_Z_MIN, TIME_UNPACK_Z_ALL( I ) )
         TIME_UNPACK_Z_MAX = MAX( TIME_UNPACK_Z_MAX, TIME_UNPACK_Z_ALL( I ) )
         TIME_UNPACK_Z_SUM = TIME_UNPACK_Z_SUM + TIME_UNPACK_Z_ALL( I )
      END DO
      TIME_UNPACK_Z_AVG = TIME_UNPACK_Z_SUM / NUMPES

      IF ( MG_PERF(mype+1)%NUM_SUMGRID /= 0) THEN
         TIME_SUMGRID_SUM = TIME_SUMGRID_ALL(1)
         TIME_SUMGRID_MAX = TIME_SUMGRID_ALL(1)
         TIME_SUMGRID_MIN = TIME_SUMGRID_ALL(1)
         DO I = 2, NUMPES
            TIME_SUMGRID_MIN = MIN(TIME_SUMGRID_MIN, TIME_SUMGRID_ALL(I))
            TIME_SUMGRID_MAX = MAX(TIME_SUMGRID_MAX, TIME_SUMGRID_ALL(I))
            TIME_SUMGRID_SUM = TIME_SUMGRID_SUM + TIME_SUMGRID_ALL(I)
         END DO
         TIME_SUMGRID_AVG = TIME_SUMGRID_SUM/NUMPES

         TIME_SUMGRID_COMP_SUM = TIME_SUMGRID_COMP_ALL(1)
         TIME_SUMGRID_COMP_MAX = TIME_SUMGRID_COMP_ALL(1)
         TIME_SUMGRID_COMP_MIN = TIME_SUMGRID_COMP_ALL(1)
         DO I = 2, NUMPES
            TIME_SUMGRID_COMP_MIN = MIN(TIME_SUMGRID_COMP_MIN, TIME_SUMGRID_COMP_ALL(I))
            TIME_SUMGRID_COMP_MAX = MAX(TIME_SUMGRID_COMP_MAX, TIME_SUMGRID_COMP_ALL(I))
            TIME_SUMGRID_COMP_SUM = TIME_SUMGRID_COMP_SUM + TIME_SUMGRID_COMP_ALL(I)
         END DO
         TIME_SUMGRID_COMP_AVG = TIME_SUMGRID_COMP_SUM/NUMPES

         TIME_SUMGRID_COMM_SUM = TIME_SUMGRID_COMM_ALL(1)
         TIME_SUMGRID_COMM_MAX = TIME_SUMGRID_COMM_ALL(1)
         TIME_SUMGRID_COMM_MIN = TIME_SUMGRID_COMM_ALL(1)
         DO I = 2, NUMPES
            TIME_SUMGRID_COMM_MIN = MIN(TIME_SUMGRID_COMM_MIN, TIME_SUMGRID_COMM_ALL(I))
            TIME_SUMGRID_COMM_MAX = MAX(TIME_SUMGRID_COMM_MAX, TIME_SUMGRID_COMM_ALL(I))
            TIME_SUMGRID_COMM_SUM = TIME_SUMGRID_COMM_SUM + TIME_SUMGRID_COMM_ALL(I)
         END DO
         TIME_SUMGRID_COMM_AVG = TIME_SUMGRID_COMM_SUM/NUMPES
      END IF

      CALL DATE_AND_TIME ( TEST_DATE, TEST_TIME )

#if defined ( _TIMER_SYSTEM_CLOCK )
      CALL SYSTEM_CLOCK ( IDUM, ICLOCK_RATE )
      CLOCK_RES = REAL(ICLOCK_RATE)
#elif defined ( _TIMER_MPI )
      CLOCK_RES = REAL(MPI_WTICK ( ))
#endif


      DO J = 1, 2

         ! Write data to stdout and file:

         IF ( J == 1 ) THEN
            OUTPUT_LOC = 6 ! STDOUT
         ELSE IF ( J == 2 ) THEN
            OUTPUT_LOC = 8 
            OPEN ( UNIT=8, FILE = 'results.txt' )
         END IF

         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,100)
         WRITE(OUTPUT_LOC,*) '          Mantevo miniapp MiniGhost experiment'
         WRITE(OUTPUT_LOC,100)
   
         WRITE(OUTPUT_LOC,*)
         SELECT CASE ( COMM_METHOD )
            CASE ( COMM_METHOD_BSPMA )
   
               WRITE(OUTPUT_LOC,*) 'Communication strategy: full message aggregation (COMM_METHOD_BSPMA)'
   
            CASE ( COMM_METHOD_SVAF )
   
               WRITE(OUTPUT_LOC,*) 'Communication strategy: one variable at a time (COMM_METHOD_SVAF)'
   
            CASE ( COMM_METHOD_SVCP )
   
               WRITE(OUTPUT_LOC,*) 'Communication strategy: comp/comm ovrelap (COMM_METHOD_SVCP)'
   
            CASE DEFAULT
   
               WRITE(OUTPUT_LOC,*) '** Warning ** Unknown communication strategy'
   
         END SELECT
         WRITE(OUTPUT_LOC,*)
   
         SELECT CASE ( STENCIL )
   
            CASE ( STENCIL_NONE )
   
               WRITE(OUTPUT_LOC,*) 'No computation inserted.'
   
            CASE ( STENCIL_2D5PT )
   
               WRITE(OUTPUT_LOC,*) 'Computation: 5 pt difference stencil on a 2D grid (STENCIL_2D5PT)'
   
            CASE ( STENCIL_2D9PT )
   
               WRITE(OUTPUT_LOC,*) 'Computation: 9 pt difference stencil on a 2D grid (STENCIL_2D9PT)'
   
            CASE ( STENCIL_3D7PT )
   
               WRITE(OUTPUT_LOC,*) 'Computation: 7 pt difference stencil on a 3D grid (STENCIL_3D7PT)'
   
            CASE ( STENCIL_3D27PT )
   
               WRITE(OUTPUT_LOC,*) 'Computation: 27 pt difference stencil on a 3D grid stencil (STENCIL_3D27PT)'
   
            CASE DEFAULT
   
               WRITE(OUTPUT_LOC,*) '** Warning ** Unknown computation'
   
         END SELECT
   
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,101) NX * NPX, NY * NPY, NZ * NPZ
         WRITE(OUTPUT_LOC,102) NX, NY, NZ
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,103) NUM_VARS
         WRITE(OUTPUT_LOC,104) linfo(mype+1)%NUM_SUM_GRID, PERCENT_SUM
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,110) NUM_TSTEPS
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,120) NPX, NPY, NPZ
         WRITE(OUTPUT_LOC,*)
         IF ( SCALING == SCALING_STRONG ) THEN   ! Not that it really matters.
            WRITE(OUTPUT_LOC,*) 'MPI version, strong scaling'
         ELSE
            WRITE(OUTPUT_LOC,*) 'MPI version, weak scaling'
         END IF
         WRITE(OUTPUT_LOC,*)
         IF ( NUMPES == 1 ) THEN
            WRITE(OUTPUT_LOC,121) TEST_MACHINE, TEST_TIME, TEST_DATE
         ELSE
            WRITE(OUTPUT_LOC,122) NUMPES, TEST_MACHINE, TEST_TIME, TEST_DATE
         END IF
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,130) TIME_WALL_MAX, CLOCK_RES
         WRITE(OUTPUT_LOC,*)
   
         IF ( .NOT.REPORT_PROFILING ) THEN
            WRITE(OUTPUT_LOC,200)
            RETURN 
         END IF
   
         IF ( STENCIL /= STENCIL_NONE ) THEN
   
            GFLOPS = (REAL(NUM_TSTEPS) * REAL(GNUM_ADDS + GNUM_MULTS) / TIME_WALL_MAX) / GIGA
   
            WRITE(OUTPUT_LOC,*) '-------------------------------------------------'
            WRITE(OUTPUT_LOC,*) '          Computational performance:'
            WRITE(OUTPUT_LOC,*) '-------------------------------------------------'
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,140) TIME_STENCIL_AVG, TIME_STENCIL_MIN, TIME_STENCIL_MAX
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,131) '       Total GFLOPS:               ', GFLOPS
            WRITE(OUTPUT_LOC,131) '       GFLOPS per process:         ', GFLOPS / NUMPES
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,131) '       Total floating point ops:   ', REAL(NUM_TSTEPS) * REAL(GNUM_ADDS + GNUM_MULTS)
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,131) '          Sums:                    ', REAL(NUM_TSTEPS) * REAL(GNUM_ADDS)
            WRITE(OUTPUT_LOC,131) '          Mult:                    ', REAL(NUM_TSTEPS) * REAL(GNUM_MULTS)
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,*) '       Number of spikes set: ', NUM_SPIKES
         ELSE
            WRITE(OUTPUT_LOC,*)  '  *** No computation *** '
         END IF
   
         IF ( NUMPES == 1 )    &
            GO TO 10
   
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*) '-------------------------------------------------'
         WRITE(OUTPUT_LOC,*) '     Inter-process communication statistics:'
         WRITE(OUTPUT_LOC,*) '-------------------------------------------------'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,150) TIME_COMMSUM_AVG, TIME_PACK_AVG, TIME_SEND_AVG, TIME_PACK_AVG, TIME_RECV_AVG, TIME_UNPACK_AVG
         WRITE(OUTPUT_LOC,151) TIME_COMMSUM_MAX, TIME_PACK_MAX, TIME_SEND_MAX, TIME_PACK_MAX, TIME_RECV_MAX, TIME_UNPACK_MAX
         WRITE(OUTPUT_LOC,152) TIME_COMMSUM_MIN, TIME_PACK_MIN, TIME_SEND_MIN, TIME_PACK_MIN, TIME_RECV_MIN, TIME_UNPACK_MIN
         WRITE(OUTPUT_LOC,*)
   
         WRITE(OUTPUT_LOC,*)  ' Messages SENT per time step:'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,131) '   Number:                         ', REAL(GNUM_SENDS)
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,132) '      Total counts/bytes per time step:   ', REAL(GSEND_COUNT), REAL(GSEND_COUNT)*SIZE_OF_DATA
         WRITE(OUTPUT_LOC,133) '      Min, max counts/bytes per time step:',       &
            REAL(GSEND_COUNT_MIN), REAL(GSEND_COUNT_MIN)*SIZE_OF_DATA, &
            REAL(GSEND_COUNT_MAX), REAL(GSEND_COUNT_MAX)*SIZE_OF_DATA
         WRITE(OUTPUT_LOC,134) '      Min, max number/counts/bytes per time step per process:',       &
            GSEND_NODE_NUM_MIN, REAL(GSEND_NODE_COUNT_MIN), &
            REAL(GSEND_NODE_COUNT_MIN)*SIZE_OF_DATA, &
            GSEND_NODE_NUM_MAX, REAL(GSEND_NODE_COUNT_MAX), &
            REAL(GSEND_NODE_COUNT_MAX)*SIZE_OF_DATA
   
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*)  ' Messages RECEIVED per time step'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,131) '   Number:                         ', REAL(GNUM_RECVS)
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,132) '      Total counts/bytes per time step:   ', REAL(GRECV_COUNT), REAL(GRECV_COUNT)*SIZE_OF_DATA
         WRITE(OUTPUT_LOC,133) '      Min, max counts/bytes per time step:',       &
            REAL(GRECV_COUNT_MIN), REAL(GRECV_COUNT_MIN)*SIZE_OF_DATA, &
            REAL(GRECV_COUNT_MAX), REAL(GRECV_COUNT_MAX)*SIZE_OF_DATA
         WRITE(OUTPUT_LOC,134) '      Min, max number/counts/bytes per time step per process:',       &
            GRECV_NODE_NUM_MIN, REAL(GRECV_NODE_COUNT_MIN), &
            REAL(GRECV_NODE_COUNT_MIN)*SIZE_OF_DATA, &
            GRECV_NODE_NUM_MAX, REAL(GRECV_NODE_COUNT_MAX), &
            REAL(GRECV_NODE_COUNT_MAX)*SIZE_OF_DATA
   
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*) '     X-direction inter-process communication statistics:'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,150) TIME_COMMSUM_X_AVG, TIME_PACK_X_AVG, TIME_SEND_X_AVG, TIME_WAIT_X_AVG, TIME_RECV_X_AVG, &
                               TIME_UNPACK_X_AVG
         WRITE(OUTPUT_LOC,151) TIME_COMMSUM_X_MAX, TIME_PACK_X_MAX, TIME_SEND_X_MAX, TIME_WAIT_X_MAX, TIME_RECV_X_MAX, &
                               TIME_UNPACK_X_MAX
         WRITE(OUTPUT_LOC,152) TIME_COMMSUM_X_MIN, TIME_PACK_X_MIN, TIME_SEND_X_MIN, TIME_WAIT_X_MIN, TIME_RECV_X_MIN, &
                               TIME_UNPACK_X_MIN
   
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*) '     Y-direction inter-process communication statistics:'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,150) TIME_COMMSUM_Y_AVG, TIME_PACK_Y_AVG, TIME_SEND_Y_AVG, TIME_WAIT_Y_AVG, TIME_RECV_Y_AVG, &
                               TIME_UNPACK_Y_AVG
         WRITE(OUTPUT_LOC,151) TIME_COMMSUM_Y_MAX, TIME_PACK_Y_MAX, TIME_SEND_Y_MAX, TIME_WAIT_Y_MAX, TIME_RECV_Y_MAX, &
                               TIME_UNPACK_Y_MAX
         WRITE(OUTPUT_LOC,152) TIME_COMMSUM_Y_MIN, TIME_PACK_Y_MIN, TIME_SEND_Y_MIN, TIME_WAIT_Y_MIN, TIME_RECV_Y_MIN, &
                               TIME_UNPACK_Y_MIN
   
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*) '     Z-direction inter-process communication statistics:'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,150) TIME_COMMSUM_Z_AVG, TIME_PACK_Z_AVG, TIME_SEND_Z_AVG, TIME_WAIT_Z_AVG, TIME_RECV_Z_AVG, &
                               TIME_UNPACK_Z_AVG
         WRITE(OUTPUT_LOC,151) TIME_COMMSUM_Z_MAX, TIME_PACK_Z_MAX, TIME_SEND_Z_MAX, TIME_WAIT_Z_MAX, TIME_RECV_Z_MAX, &
                               TIME_UNPACK_Z_MAX
         WRITE(OUTPUT_LOC,152) TIME_COMMSUM_Z_MIN, TIME_PACK_Z_MIN, TIME_SEND_Z_MIN, TIME_WAIT_Z_MIN, TIME_RECV_Z_MIN, &
                               TIME_UNPACK_Z_MIN
         WRITE(OUTPUT_LOC,*)
   
    10   CONTINUE

         IF ( MG_PERF(mype+1)%NUM_SUMGRID /= 0) THEN
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,*) '  GRIDSUM performance'
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,131) '   Number:                         ', REAL(MG_PERF(mype+1)%NUM_SUMGRID)
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,161) TIME_SUMGRID_AVG, TIME_SUMGRID_COMP_AVG, TIME_SUMGRID_COMM_AVG
            WRITE(OUTPUT_LOC,162) TIME_SUMGRID_MAX, TIME_SUMGRID_COMP_MAX, TIME_SUMGRID_COMM_MAX
            WRITE(OUTPUT_LOC,163) TIME_SUMGRID_MIN, TIME_SUMGRID_COMP_MIN, TIME_SUMGRID_COMM_MIN
            WRITE(OUTPUT_LOC,*)
         ENDIF
   
         IF ( NUMPES == 1 )    &
            GO TO 20
   
         IF ( MG_PERF(mype+1)%NUM_ALLREDUCES /= 0 ) THEN
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,*)  ' Reductions (to all) per time step'
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,131) '    Number:                  ', REAL(MG_PERF(mype+1)%NUM_ALLREDUCES)
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,132) '      Total counts/bytes per time step:',       &
               REAL(MG_PERF(mype+1)%ALLREDUCE_COUNT), REAL(MG_PERF(mype+1)%ALLREDUCE_COUNT)*SIZE_OF_DATA
            WRITE(OUTPUT_LOC,133) '      Min, max counts/bytes per time step: ',       &
               REAL(GALLREDUCE_COUNT_MIN), REAL(GALLREDUCE_COUNT_MIN)*SIZE_OF_DATA, &
               REAL(GALLREDUCE_COUNT_MAX), REAL(GALLREDUCE_COUNT_MAX)*SIZE_OF_DATA
         END IF
   
         IF ( MG_PERF(mype+1)%NUM_BCASTS /= 0 ) THEN
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,*)  ' Broadcasts per time step'
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,131) '    Number:                  ', REAL(MG_PERF(mype+1)%NUM_BCASTS)
            WRITE(OUTPUT_LOC,*)
            WRITE(OUTPUT_LOC,133) '       Total counts/bytes per time step: ',       &
               REAL(MG_PERF(mype+1)%BCAST_COUNT), REAL(MG_PERF(mype+1)%BCAST_COUNT)*SIZE_OF_DATA
            WRITE(OUTPUT_LOC,133) '       Min, max counts/bytes per time step: ',       &
               REAL(GBCAST_COUNT_MIN), REAL(GBCAST_COUNT_MIN)*SIZE_OF_DATA, &
               REAL(GBCAST_COUNT_MAX), REAL(GBCAST_COUNT_MAX)*SIZE_OF_DATA
         END IF
    
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*) ' Per process communication time:'
         WRITE(OUTPUT_LOC,*)
         WRITE(OUTPUT_LOC,*) '    PE     Wall (sec)     % Comm'
         WRITE(OUTPUT_LOC,*) '   ----    ----------     ------'
         DO I = 1, NUMPES
            IF ( TIME_WALL_ALL( I ) == TIME_WALL_MAX ) THEN
               WRITE(OUTPUT_LOC,171) I-1, TIME_WALL_ALL( I ),                   &
                  ( TIME_COMM_ALL( I ) / TIME_WALL_ALL( I ) ) * 100.0
            ELSE IF ( TIME_WALL_ALL( I ) == TIME_WALL_MIN ) THEN
               WRITE(OUTPUT_LOC,172) I-1, TIME_WALL_ALL( I ),                   &
                  ( TIME_COMM_ALL( I ) / TIME_WALL_ALL( I ) ) * 100.0
            ELSE
               WRITE(OUTPUT_LOC,173) I-1, TIME_WALL_ALL( I ),                   &
                  ( TIME_COMM_ALL( I ) / TIME_WALL_ALL( I ) ) * 100.0
            END IF
         END DO
         WRITE(OUTPUT_LOC,*) 
         WRITE(OUTPUT_LOC,200)
         WRITE(OUTPUT_LOC,*)
   
    20   CONTINUE

         IF ( OUTPUT_LOC == 8 ) &
            CLOSE ( UNIT = 8 )

      END DO

      ! Format statements

 100  FORMAT ( ' =================================================' )

 101  FORMAT ( '      Global Grid Dimension: ', I8, ', ', I8, ', ', I8 )
 102  FORMAT ( '      Local Grid Dimension : ', I8, ', ', I8, ', ', I8 )

 103  FORMAT ( ' Number of variables: ', I2 )
 104  FORMAT( ' Number of variables reduced each time step: ', I2, '; requested  ', I3, '%.')

 110  FORMAT ( '      Time steps: ', I6 )

 120  FORMAT ( '      Task grid: ', I5, ',', I5, ',', I5 )

 121  FORMAT ( ' 1 process executing on machine ', A30,  // &
             ' Program execution at ', A10, ' on ', A8, '.' )

 122  FORMAT ( I4, ' processes executing on machine ', A30,  // &
             ' Program execution at ', A10, ' on ', A8, '.' )

 130  FORMAT ( ' Total time for test (sec): ',  1PE9.3, '; clock resolution is ', 1PE9.3 ,' per second.' )

 131  FORMAT ( A30, 1PE12.3 )
 132  FORMAT ( A44, 1PE12.3, 1PE12.3 )
 133  FORMAT ( A44, 1PE12.3, ', ', 1PE12.3, '; ', 1PE12.3, ', ', 1PE12.3 )
 134  FORMAT ( A63, I8, ', ', 1PE12.3, ', ', 1PE12.3, '; ', I8, ', ', 1PE12.3, ', ', 1PE12.3 )

 140  FORMAT ( '       Time : avg, min, max secs:', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3 )

 150  FORMAT ( '       Avg (total,pack,send,wait,recv,unpack secs): ', 1PE9.3, ', ', 1PE9.3, ', ', &
               1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3 )
 151  FORMAT ( '       Max (total,pack,send,wait,recv,unpack secs): ', 1PE9.3, ', ', 1PE9.3, ', ', &
               1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3 )
 152  FORMAT ( '       Min (total,pack,send,wait,recv,unpack secs): ', 1PE9.3, ', ', 1PE9.3, ', ', &
               1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3 )

 161  FORMAT ( '       Avg (total,compute,reduce secs): ', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3)
 162  FORMAT ( '       Max (total,compute,reduce secs): ', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3)
 163  FORMAT ( '       Min (total,compute,reduce secs): ', 1PE9.3, ', ', 1PE9.3, ', ', 1PE9.3)

 ! Comm reporting.
 171  FORMAT ( '     ', I3, '     ', 0PE9.3, '     ', F9.3, ' (Max wallclock)' )
 172  FORMAT ( '     ', I3, '     ', 0PE9.3, '     ', F9.3, ' (Min wallclock)' )
 173  FORMAT ( '     ', I3, '     ', 0PE9.3, '     ', F9.3 )

 200  FORMAT ( ' ================== End report ===================' )


      END SUBROUTINE PERF_REPORT

END MODULE MG_PROFILING_MOD
