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

   recursive SUBROUTINE MINI_GHOST ( SCALING_IN,         &
                           NX_IN,              &
                           NY_IN,              &
                           NZ_IN,              &
                           NUM_VARS_IN,        &
                           PERCENT_SUM_IN,     &
                           NUM_SPIKES_IN,      &
                           NUM_TSTEPS_IN,      &
                           STENCIL_IN,         &
                           COMM_METHOD_IN,     &
                           SEND_PROTOCOL_IN,   &
                           CHECK_DIFFUSION_IN, &
                           NPX_IN,             &
                           NPY_IN,             &
                           NPZ_IN,             &
                           PROFILING_IN,       &
                           CP_INTERVAL_IN,     &
                           CP_FILE_IN          &
                           )

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD
   USE MG_UTILS_MOD
   USE MG_BUFINIT_MOD
   USE DRIVER_BSPMA_MOD
   USE DRIVER_SVAF_MOD
   USE DRIVER_SVCP_MOD
   USE MG_STENCIL_MOD
   USE MG_SVCP_INIT_MOD
   USE MG_SVCP_MOD
   USE MG_PROFILING_MOD
   USE MG_SUM_GRID_MOD
   USE MG_CHECKPOINT_MOD

   IMPLICIT NONE

      INTEGER(KIND=MG_INT), INTENT(IN) :: &
         SCALING_IN,         &
         NX_IN,              &
         NY_IN,              &
         NZ_IN,              &
         NUM_VARS_IN,        &
         PERCENT_SUM_IN,     &
         NUM_SPIKES_IN,      &
         NUM_TSTEPS_IN,      &
         STENCIL_IN,         &
         COMM_METHOD_IN,     &
         SEND_PROTOCOL_IN,   &
         CHECK_DIFFUSION_IN, &
         NPX_IN,             &
         NPY_IN,             &
         NPZ_IN,             &
         PROFILING_IN,       &
         CP_INTERVAL_IN


      CHARACTER*(1024), INTENT(IN) :: &
         CP_FILE_IN

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         IERR,                    &  ! Return status
         I,                       &  ! Counter
         IVAR,   &                   ! Counter (over variables)
         mype, world


      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &
         TIME_START_2,            &
         TIME_START_ALL



      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      PROFILE_MG = .FALSE.

      ! Set C input parameters to Fortran-stored parameters
      SCALING       = SCALING_IN
      NX            = NX_IN
      NY            = NY_IN
      NZ            = NZ_IN
      NUM_VARS      = NUM_VARS_IN
      PERCENT_SUM   = PERCENT_SUM_IN
      NUM_SPIKES    = NUM_SPIKES_IN
      NUM_TSTEPS    = NUM_TSTEPS_IN
      STENCIL       = STENCIL_IN
      COMM_METHOD   = COMM_METHOD_IN
      SEND_PROTOCOL = SEND_PROTOCOL_IN
      IF ( CHECK_DIFFUSION_IN == 0 ) THEN
         CHECK_DIFFUSION = .FALSE.
      ELSE
         CHECK_DIFFUSION = .TRUE.
      END IF
      NPX           = NPX_IN
      NPY           = NPY_IN
      NPZ           = NPZ_IN

      IF ( PROFILING_IN == 1 ) THEN
         PROFILE_MG = .TRUE.
         REPORT_PROFILING = .TRUE.
      ELSE
         PROFILE_MG = .FALSE.
         REPORT_PROFILING = .FALSE.
      END IF

      CP_INTERVAL = CP_INTERVAL_IN
      CP_FILE     = CP_FILE_IN


      ! Parallel processing configuration:

#ifdef USING_TPM

	NUMPES = TPM_Comm_size()
	mype = TPM_Comm_rank()
	world = TPM_COMM_WORLD()
#else

	 call MPI_Comm_size(MPI_COMM_WORLD, NUMPES, IERR)
	 call MPI_Comm_rank(MPI_COMM_WORLD, mype, IERR)
	 world = MPI_COMM_WORLD
#endif

    !  CALL MPI_COMM_SIZE ( MPI_COMM_WORLD(), NUMPES, IERR )
    !  CALL MPI_COMM_RANK ( MPI_COMM_WORLD(), mype, IERR )

      if(mype == 0) THEN
      	 allocate(linfo(NUMPES) )
      	 allocate(winfo(NUMPES) )
 	 	 allocate(MG_PERF(NUMPES))
	  END IF

	  CALL MPI_Barrier(world, ierr )

       linfo(mype+1)%MYPE = mype

      !CALL MPI_COMM_DUP ( MPI_COMM_WORLD(), linfo(mype+1)%MPI_COMM_MG, IERR )

      linfo(mype+1)%MPI_COMM_MG = world

      CALL MPI_ERRHANDLER_SET ( linfo(mype+1)%MPI_COMM_MG, MPI_ERRORS_ARE_FATAL, IERR )





      ! Set 3d processor grid, position in processor grid.
      ! Set neighbors, message tag initialization, spikes (for testing).
      CALL INIT ( IERR, mype )

      ! Initialize performance capture
      CALL PERF_INIT ( IERR, mype )

      ! Allocate local domain, including space for all variables, including ghosts:
      CALL GRID_INIT ( IERR, mype )

      CALL PRINT_HEADER ( COMM_METHOD, STENCIL, IERR, mype )

      CALL MG_BUFINIT ( IERR, mype )
      CALL CHECK_ERROR ( IERR, 'MINIGHOST: MG_BUFINIT', IERR, mype )

      ! ---------------------------------
      ! Call COMM_METHOD specific DRIVER.
      ! ---------------------------------

      SELECT CASE ( COMM_METHOD )

         CASE ( COMM_METHOD_BSPMA )

            CALL DRIVER_BSPMA ( IERR, mype )

         CASE ( COMM_METHOD_SVAF )

            CALL DRIVER_SVAF ( IERR, mype )

         CASE ( COMM_METHOD_SVCP )

            CALL DRIVER_SVCP ( IERR, mype )

      END SELECT ! COMM_METHOD

      !  Report results.

      CALL PERF_REPORT ( COMM_METHOD, STENCIL, IERR, mype )
      CALL CHECK_ERROR ( IERR, 'MINIGHOST: PERF_REPORT', COMM_METHOD, mype )

      ! Done. Deallocate memory and terminate.

      CALL GRID_DEALLOC ( IERR, mype )

      IF ( ALLOCATED ( linfo(mype+1)%SEND_BUFFER_BACK ) )  &
         DEALLOCATE ( linfo(mype+1)%SEND_BUFFER_BACK )
      IF ( ALLOCATED ( linfo(mype+1)%RECV_BUFFER_BACK ) )  &
         DEALLOCATE ( linfo(mype+1)%RECV_BUFFER_BACK )
      IF ( ALLOCATED ( linfo(mype+1)%SEND_BUFFER_FRONT ) ) &
         DEALLOCATE ( linfo(mype+1)%SEND_BUFFER_FRONT )
      IF ( ALLOCATED ( linfo(mype+1)%RECV_BUFFER_FRONT ) ) &
         DEALLOCATE ( linfo(mype+1)%RECV_BUFFER_FRONT )
      IF ( ALLOCATED ( linfo(mype+1)%SEND_BUFFER_EAST ) )  &
         DEALLOCATE ( linfo(mype+1)%SEND_BUFFER_EAST )
      IF ( ALLOCATED ( linfo(mype+1)%RECV_BUFFER_EAST ) )  &
         DEALLOCATE ( linfo(mype+1)%RECV_BUFFER_EAST )
      IF ( ALLOCATED ( linfo(mype+1)%SEND_BUFFER_WEST ) )  &
         DEALLOCATE ( linfo(mype+1)%SEND_BUFFER_WEST )
      IF ( ALLOCATED ( linfo(mype+1)%RECV_BUFFER_WEST ) )  &
         DEALLOCATE ( linfo(mype+1)%RECV_BUFFER_WEST )
      IF ( ALLOCATED ( linfo(mype+1)%SEND_BUFFER_NORTH ) ) &
         DEALLOCATE ( linfo(mype+1)%SEND_BUFFER_NORTH )
      IF ( ALLOCATED ( linfo(mype+1)%RECV_BUFFER_NORTH ) ) &
         DEALLOCATE ( linfo(mype+1)%RECV_BUFFER_NORTH )
      IF ( ALLOCATED ( linfo(mype+1)%SEND_BUFFER_SOUTH ) ) &
         DEALLOCATE ( linfo(mype+1)%SEND_BUFFER_SOUTH )
      IF ( ALLOCATED ( linfo(mype+1)%RECV_BUFFER_SOUTH ) ) &
         DEALLOCATE ( linfo(mype+1)%RECV_BUFFER_SOUTH )

      IF ( ALLOCATED ( linfo(mype+1)%MSG_REQS ) ) &
         DEALLOCATE ( linfo(mype+1)%MSG_REQS )
      IF ( ALLOCATED ( linfo(mype+1)%MSG_TAGS ) ) &
         DEALLOCATE ( linfo(mype+1)%MSG_TAGS )

      CALL TERMINATE ( IERR )

   END SUBROUTINE MINI_GHOST
