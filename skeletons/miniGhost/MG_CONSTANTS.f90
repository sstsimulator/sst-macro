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
 
MODULE MG_CONSTANTS_MOD

   IMPLICIT NONE

   include 'sstmac/mpi/sstmac_mpi_f.h'
   include 'sstmac/software/libraries/compute/sstmac_compute_f.h'
#ifdef USING_TPM
   include 'tpm_f.h'
#endif

   ! Default types should be used unless otherwise required (e.g. MPI_Wtime is REAL8).
   ! Fortran datatype parameters

   INTEGER(KIND=1_4), PARAMETER :: MG_INT4 = 4 
   INTEGER(KIND=1_4), PARAMETER :: MG_INT8 = 8

   INTEGER(KIND=1_4), PARAMETER :: MG_REAL4 = 4
   INTEGER(KIND=1_4), PARAMETER :: MG_REAL8 = 8

#if defined ( _INT4 )
   INTEGER(KIND=1_4), PARAMETER :: MG_INT  = 4
   INTEGER(MG_INT4), PARAMETER :: MG_MPI_INT = MPI_INTEGER4
#elif defined ( _INT8 )
   INTEGER(KIND=1_4), PARAMETER :: MG_INT = 8
   INTEGER(MG_INT4), PARAMETER :: MG_MPI_INT = MPI_INTEGER8
#else
   Set INTEGER precision: -D_INT4 or -D_INT8. Default (INT4) was over-ridden?
#endif

#if defined ( _REAL4 )
   INTEGER(KIND=1_4), PARAMETER :: MG_REAL = 4
   INTEGER(MG_INT4), PARAMETER :: MG_MPI_REAL = MPI_REAL4
#elif defined ( _REAL8 )
   INTEGER(KIND=1_4), PARAMETER :: MG_REAL = 8
   INTEGER(MG_INT4), PARAMETER :: MG_MPI_REAL = MPI_REAL8
#else
   Set REAL precision: -D_REAL4 or -D_REAL8. Default (REAL8) was over-ridden?
#endif

   REAL(KIND=MG_REAL), PARAMETER ::        &
      EPS           = 1.0E-4,              &
      GIGA          = 1000000000.0,        &
      FIFTH         = 0.20,                &
      SEVENTH       = 1.0 / 7.0,           &
      NINTH         = 1.0 / 9.0,           &
      TWENTYSEVENTH = 1.0 / 27.0,          &
      ONE_THOUSAND  = 1000.0,              &
      ZERO          = 0.0

   ! ---------
   ! Variables
   ! ---------

   LOGICAL ::                       &
      PROFILE_MG       = .FALSE.,   &   ! Gathering. (Turned off after one tstep.)
      REPORT_PROFILING = .FALSE.        ! Reporting. 
   

   INTEGER(KIND=MG_INT) :: ROOT_PE, &  ! MPI rank that reads/writes.
         NUMPES                         ! Number of MPI ranks.


TYPE :: LOCAL_INFO

     INTEGER(KIND=MG_INT) ::          &

      MY_GLOBAL_NX_START,           &
      MY_GLOBAL_NY_START,           &
      MY_GLOBAL_NZ_START,           &  ! Global location.
      MY_GLOBAL_NX_END,             &
      MY_GLOBAL_NY_END,             &
      MY_GLOBAL_NZ_END,             &  ! Global location.
      NUM_NEIGHS,                   &  ! Number of parallel process (rank) neighbors.
      NUM_SUM_GRID,                 &  ! Number of variables globally summed each time step.

      ! Parallel env parameters.
      MYPE,                         &  ! MPI rank.

      MPI_COMM_MG,                  &  ! Duplicate of MPI_COMM_WORLD

      MYPX, MYPY, MYPZ                 ! Processor position in the processor grid.

     INTEGER(KIND=MG_INT),   DIMENSION(:), ALLOCATABLE ::      &

      MSG_REQS,                    &
      MSG_TAGS,                    &
      NEIGHBORS,                   &
      NEIGHBORS_ORIG,              &  ! Temporary storage for DIAG exchanges. May be needed for AMR?
      OFFSET_RECV_BUFFER,          &

      VAR_PTR

       INTEGER(KIND=MG_INT), DIMENSION(:,:), ALLOCATABLE ::   &
      SPIKE_LOC                        ! Location of spike.

      LOGICAL, DIMENSION(:), ALLOCATABLE :: &
      GRIDS_TO_SUM                     ! List of GRID arrays to sum.

        REAL(KIND=MG_REAL),   DIMENSION(:), ALLOCATABLE ::   &
      GSUM_OLD                         ! Maintain previous time step's GRID sums.

       REAL(KIND=MG_REAL),   DIMENSION(:,:), ALLOCATABLE ::   &
      SPIKES       ! Heat spike to be inserted.


       ! ------------------------------
   ! Message buffer workspace, etc.
   ! ------------------------------

   INTEGER(KIND=MG_INT) ::       &

      TSTEP,                      &  ! Time step counter.

      MAX_NUM_SENDS,              &
      MAX_NUM_RECVS,              &

      NUM_RECVS,                  &  ! Number of recvs posted.
      NUM_SENDS,                  &

      COUNT_RECV_BACK,            &
      COUNT_RECV_FRONT,           &
      COUNT_RECV_EAST,            &
      COUNT_RECV_WEST,            &
      COUNT_RECV_NORTH,           &
      COUNT_RECV_SOUTH,           &

      COUNT_SEND_BACK,            &
      COUNT_SEND_FRONT,           &
      COUNT_SEND_EAST,            &
      COUNT_SEND_WEST,            &
      COUNT_SEND_NORTH,           &
      COUNT_SEND_SOUTH,           &

      RECV_BUFFER_NORTH_SIZE,     &
      RECV_BUFFER_SOUTH_SIZE,     &
      RECV_BUFFER_EAST_SIZE,      &
      RECV_BUFFER_WEST_SIZE,      &
      RECV_BUFFER_BACK_SIZE,      &
      RECV_BUFFER_FRONT_SIZE,     &

      SEND_BUFFER_SIZE,           & ! For blocking protocol

      SEND_BUFFER_NORTH_SIZE,     & ! For nonblocking protocol
      SEND_BUFFER_SOUTH_SIZE,     & ! For nonblocking protocol
      SEND_BUFFER_EAST_SIZE,      & ! For nonblocking protocol
      SEND_BUFFER_WEST_SIZE,      & ! For nonblocking protocol
      SEND_BUFFER_BACK_SIZE,      & ! For nonblocking protocol
      SEND_BUFFER_FRONT_SIZE,     & ! For nonblocking protocol

      MSG_REQ_OFFSET,             & ! For MSMA and SVAF

      REQ_OFFSET_RECV_BACK,       & ! For SVCP versions
      REQ_OFFSET_RECV_FRONT,      & ! For SVCP versions
      REQ_OFFSET_RECV_EAST,       & ! For SVCP versions
      REQ_OFFSET_RECV_WEST,       & ! For SVCP versions
      REQ_OFFSET_RECV_NORTH,      & ! For SVCP versions
      REQ_OFFSET_RECV_SOUTH,      & ! For SVCP versions

      REQ_OFFSET_SEND_BACK ,      & ! For SVCP versions
      REQ_OFFSET_SEND_FRONT,      & ! For SVCP versions
      REQ_OFFSET_SEND_EAST ,      & ! For SVCP versions
      REQ_OFFSET_SEND_WEST ,      & ! For SVCP versions
      REQ_OFFSET_SEND_NORTH,      & ! For SVCP versions
      REQ_OFFSET_SEND_SOUTH         ! For SVCP versions



   REAL(KIND=MG_REAL),   DIMENSION(:), ALLOCATABLE ::      &

      RECV_BUFFER_BACK,            &
      RECV_BUFFER_FRONT,           &
      RECV_BUFFER_EAST,            &
      RECV_BUFFER_WEST,            &
      RECV_BUFFER_NORTH,           &
      RECV_BUFFER_SOUTH,           &

      SEND_BUFFER,                 & ! For blocking protocol

      SEND_BUFFER_BACK,            & ! For nonblocking protocol
      SEND_BUFFER_FRONT,           & ! For nonblocking protocol
      SEND_BUFFER_EAST,            & ! For nonblocking protocol
      SEND_BUFFER_WEST,            & ! For nonblocking protocol
      SEND_BUFFER_NORTH,           & ! For nonblocking protocol
      SEND_BUFFER_SOUTH              ! For nonblocking protocol

 END TYPE LOCAL_INFO


TYPE :: WORK_INFO

 REAL(KIND=MG_REAL),   DIMENSION(:,:,:),  ALLOCATABLE ::   &
      GRID1,  GRID2,  GRID3,  GRID4,  GRID5,  GRID6,  GRID7,  GRID8,  GRID9,  GRID10,     & !
      GRID11, GRID12, GRID13, GRID14, GRID15, GRID16, GRID17, GRID18, GRID19, GRID20,     & !
      GRID21, GRID22, GRID23, GRID24, GRID25, GRID26, GRID27, GRID28, GRID29, GRID30,     & !
      GRID31, GRID32, GRID33, GRID34, GRID35, GRID36, GRID37, GRID38, GRID39, GRID40,     & !
      WORK

      END TYPE WORK_INFO

  TYPE(LOCAL_INFO), dimension(:), allocatable :: linfo
	TYPE(WORK_INFO), dimension(:), allocatable :: winfo

   REAL(KIND=MG_REAL), PARAMETER :: &
      ERROR_TOLERANCE = 0.00001          ! Difference of GRID summations of iterations.







   CHARACTER(LEN=12) ::        &
#if defined ( _PHOENIX )
      TEST_MACHINE = 'phoenix'
#elif defined ( _CHEETAH )
      TEST_MACHINE = 'cheetah'
#elif defined ( _RAM )
      TEST_MACHINE = 'ram'
#elif defined ( _JAGUAR )
      TEST_MACHINE = 'jaguar'
#elif defined ( _TIGER )
      TEST_MACHINE = 'tiger'
#elif defined ( _LULU )
      TEST_MACHINE = 'lulu'
#elif defined ( _FATMAN )
      TEST_MACHINE = 'fatman'
#elif defined ( _FIZZ )
      TEST_MACHINE = 'fizz'
#elif defined ( _ZAZA )
      TEST_MACHINE = 'zaza'
#elif defined ( _CURIE )
      TEST_MACHINE = 'curie'
#elif defined ( _MUZIA )
      TEST_MACHINE = 'muzia'
#elif defined ( _XTP )
      TEST_MACHINE = 'xt5@sandia'
#elif defined ( _OSX_MPICH )
      TEST_MACHINE = 'Mac OSX and MPICH'
#elif defined (_LINUX_CLUSTER)
      TEST_MACHINE = 'Linux Cluster'
#else
Unknown machine. Enter in CONSTANTS.F
#endif

   INTEGER(KIND=MG_INT), PARAMETER :: &

      NORTH = 1,                      &
      SOUTH = 2,                      &
      EAST  = 3,                      &
      WEST  = 4,                      &
      BACK  = 5,                      &
      FRONT = 6,                      &

      MAX_NUM_NEIGHBORS = 6

   INTEGER(KIND=MG_INT), PARAMETER ::  &

      DIR_NORTH_SOUTH = 11,            &
      DIR_EAST_WEST   = 13,            &
      DIR_BACK_FRONT  = 15

   INTEGER(KIND=MG_INT), SAVE :: &

      COUNT_NORTH = 0,            &
      COUNT_SOUTH = 0,            &
      COUNT_EAST  = 0,            &
      COUNT_WEST  = 0,            &
      COUNT_BACK  = 0,            &
      COUNT_FRONT = 0



END MODULE MG_CONSTANTS_MOD
