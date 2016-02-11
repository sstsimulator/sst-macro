
MODULE MG_UTILS_MOD

   ! Procedures included:
   !
   !   INIT
   !   PRINT_HEADER
   !   GRID_INIT
   !   TERMINATE

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD


   IMPLICIT NONE

CONTAINS
!  =================================================================================

   recursive SUBROUTINE INIT ( IERR, mype )

      IMPLICIT NONE


   
      INTEGER ::  &
         IERR, &                    ! Return status.
         mype



      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::  &
         GLOBAL_NX,            &
         GLOBAL_NY,            &
         GLOBAL_NZ,            &
         I,                    &    ! Counter.
         J,                    &    ! Counter.
         MYPE_XY,              &    ! tmp var
         NVARS,                &    ! tmp var
         REMAINDER, &
         temprank
      
      ! ------------
      ! Local Arrays
      ! ------------
 
      REAL(KIND=MG_REAL), DIMENSION(:,:), ALLOCATABLE ::   &
         RSPIKE_LOC            ! Temporary for random number generator.
   
      ! ---------------------
      ! Executable Statements 
      ! ---------------------
!	call MPI_Comm_rank(MPI_COMM_WORLD(), temprank, ierr)
! 	 WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): init() -- my actual rank is',temprank

      IERR = 0
   
      ! ---------------------------------
      ! Set position in 3d processor grid
      ! ---------------------------------

      MYPE_XY = MOD ( linfo(mype+1)%MYPE, NPX*NPY )
      linfo(mype+1)%MYPY = MYPE_XY / NPX
      REMAINDER = MOD( MYPE_XY, NPX )
      IF ( REMAINDER /= 0 ) THEN
         linfo(mype+1)%MYPX = REMAINDER
      ELSE
         linfo(mype+1)%MYPX = 0
      END IF
      linfo(mype+1)%MYPZ = linfo(mype+1)%MYPE / ( NPX*NPY )

      ! --------------
      ! Set neighbors.
      ! --------------

      ALLOCATE ( linfo(mype+1)%NEIGHBORS(MAX_NUM_NEIGHBORS), STAT = IERR )
      linfo(mype+1)%NEIGHBORS(1:MAX_NUM_NEIGHBORS) = -1

      ALLOCATE ( linfo(mype+1)%NEIGHBORS_ORIG(MAX_NUM_NEIGHBORS), STAT = IERR )

      linfo(mype+1)%NUM_NEIGHS = 0
      IF ( linfo(mype+1)%MYPY /= 0  ) THEN
         linfo(mype+1)%NEIGHBORS(SOUTH) = linfo(mype+1)%MYPE - NPX
         linfo(mype+1)%NUM_NEIGHS = linfo(mype+1)%NUM_NEIGHS + 1
      END IF
      IF ( linfo(mype+1)%MYPY /= NPY-1 ) THEN
         linfo(mype+1)%NEIGHBORS(NORTH) = linfo(mype+1)%MYPE + NPX
         linfo(mype+1)%NUM_NEIGHS = linfo(mype+1)%NUM_NEIGHS + 1
      END IF
      IF ( linfo(mype+1)%MYPX /= 0 ) THEN
         linfo(mype+1)%NEIGHBORS(WEST) = linfo(mype+1)%MYPE - 1
         linfo(mype+1)%NUM_NEIGHS = linfo(mype+1)%NUM_NEIGHS + 1
      END IF
      IF ( linfo(mype+1)%MYPX /= NPX-1 ) THEN
         linfo(mype+1)%NEIGHBORS(EAST) = linfo(mype+1)%MYPE + 1
         linfo(mype+1)%NUM_NEIGHS = linfo(mype+1)%NUM_NEIGHS + 1
      END IF
      IF ( linfo(mype+1)%MYPZ /= 0 ) THEN
         linfo(mype+1)%NEIGHBORS(BACK) = linfo(mype+1)%MYPE - ( NPX*NPY )
         linfo(mype+1)%NUM_NEIGHS = linfo(mype+1)%NUM_NEIGHS + 1
      END IF
      IF ( linfo(mype+1)%MYPZ /= NPZ-1 ) THEN
         linfo(mype+1)%NEIGHBORS(FRONT) = linfo(mype+1)%MYPE + ( NPX*NPY )
         linfo(mype+1)%NUM_NEIGHS = linfo(mype+1)%NUM_NEIGHS + 1
      END IF

      linfo(mype+1)%NEIGHBORS_ORIG = linfo(mype+1)%NEIGHBORS

      ! -------------------------------------------------------
      ! ROOT_PE generates and distributes SPIKES and SPIKE_LOC.
      ! Each GRIDi has a unique SPIKE values, but for each set,
      ! all in same location within the GRIDi array.
      ! Global location computed, PEs determines ownership.
      ! -------------------------------------------------------

      ! ALLOCATE ( linfo(mype+1)%SPIKES(NUM_VARS,NUM_SPIKES), STAT = IERR )
      ALLOCATE ( linfo(mype+1)%SPIKE_LOC(3,NUM_SPIKES), STAT=IERR )
   
      IF ( linfo(mype+1)%MYPE == ROOT_PE ) THEN
   
         GLOBAL_NX = NX * NPX
         GLOBAL_NY = NY * NPY
         GLOBAL_NZ = NZ * NPZ
         ! print *,'globals:', nx, ny, nz, GLOBAL_NX,GLOBAL_NY,GLOBAL_NZ, NPX, NPy, NPz

          ALLOCATE ( RSPIKE_LOC(3,NUM_SPIKES), STAT=IERR )

         ! CALL RANDOM_NUMBER ( linfo(mype+1)%SPIKES )
         ! linfo(mype+1)%SPIKES = linfo(mype+1)%SPIKES * GLOBAL_NX * GLOBAL_NY * GLOBAL_NZ
         ! linfo(mype+1)%spikes(1,1) = 100.0

          CALL RANDOM_NUMBER ( RSPIKE_LOC )
      
         ! First spike set to center of global grid.

         linfo(mype+1)%SPIKE_LOC(1,1) = GLOBAL_NX / 2
         linfo(mype+1)%SPIKE_LOC(2,1) = GLOBAL_NY / 2
         linfo(mype+1)%SPIKE_LOC(3,1) = GLOBAL_NZ / 2

         ! Set additional spikes randomly about global grid

         DO I = 2, NUM_SPIKES
            linfo(mype+1)%SPIKE_LOC(1,I) = RSPIKE_LOC(1,I) * GLOBAL_NX
            linfo(mype+1)%SPIKE_LOC(2,I) = RSPIKE_LOC(2,I) * GLOBAL_NY
            linfo(mype+1)%SPIKE_LOC(3,I) = RSPIKE_LOC(3,I) * GLOBAL_NZ
         END DO

         DEALLOCATE ( RSPIKE_LOC )

      END IF

      ! -----------------
      ! Distribute SPIKES
      ! -----------------
   
      CALL MPI_BCAST ( NUM_VARS*NUM_SPIKES, MG_MPI_REAL, ROOT_PE,  &
                       linfo(mype+1)%MPI_COMM_MG, IERR )
      CALL CHECK_ERROR ( IERR, 'INIT: MPI_BCAST(SPIKES)', IERR, mype)

!	call MPI_Comm_rank(MPI_COMM_WORLD(), temprank, ierr)


!	write(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): init() -- my actual rank is',temprank
!	WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): init() -- second broadcast'

      CALL MPI_BCAST ( 3*NUM_SPIKES, MPI_INTEGER, ROOT_PE,  &
                       linfo(mype+1)%MPI_COMM_MG, IERR )
      CALL CHECK_ERROR ( IERR, 'INIT: MPI_BCAST(SPIKE_LOC)', IERR, mype )

      IF ( CHECK_DIFFUSION ) THEN
         ALLOCATE ( linfo(mype+1)%GSUM_OLD(NUM_VARS), STAT=IERR )
         ! linfo(mype+1)%GSUM_OLD(1:NUM_VARS) = linfo(mype+1)%SPIKES(:,1)
      END IF

      RETURN
   
   END SUBROUTINE INIT

!  ===================================================================================
 
   SUBROUTINE PRINT_HEADER ( COMM_METHOD, STENCIL, IERR, mype )

      IMPLICIT NONE

      ! Argument Declarations
      INTEGER, INTENT(IN)  ::       &
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

      INTEGER ::            &
         I,                              &
         ICLOCK_RATE,                    &
         IDUM,                           &
         LEN

      REAL(KIND=MG_REAL4) ::       &
         CLOCK_RES = 0.0

      INTEGER(KIND=MG_INT8), PARAMETER ::  &
         SIZE_OF_DATA = 8

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      IF ( linfo(mype+1)%MYPE /= ROOT_PE ) &
         RETURN

    !  CALL DATE_AND_TIME ( TEST_DATE, TEST_TIME )

#if defined ( _TIMER_SYSTEM_CLOCK )
      CALL SYSTEM_CLOCK ( IDUM, ICLOCK_RATE )
      CLOCK_RES = REAL(ICLOCK_RATE)
#elif defined ( _TIMER_MPI )
      CLOCK_RES = REAL(MPI_WTICK ( ))
#endif

      WRITE(*,*)
      WRITE(*,100)
      WRITE(*,*) '          Mantevo miniapp MiniGhost experiment'
      WRITE(*,100)

      WRITE(*,*)
      SELECT CASE ( COMM_METHOD )
         CASE ( COMM_METHOD_BSPMA )

            WRITE(*,*) 'Communication strategy: full message aggregation (COMM_METHOD_BSPMA)'

         CASE ( COMM_METHOD_SVAF )

            WRITE(*,*) 'Communication strategy: one variable at a time (COMM_METHOD_SVAF)'

         CASE ( COMM_METHOD_SVCP )

            WRITE(*,*) 'Communication strategy: comp/comm ovrelap (COMM_METHOD_SVCP)'

         CASE DEFAULT

            WRITE(*,*) '** Warning ** Unknown communication strategy'

      END SELECT
      WRITE(*,*)

      SELECT CASE ( STENCIL )

         CASE ( STENCIL_NONE )

            WRITE(*,*) 'No computation inserted.'

         CASE ( STENCIL_2D5PT )

            WRITE(*,*) 'Computation: 5 pt difference stencil on a 2D grid (STENCIL_2D5PT)'

         CASE ( STENCIL_2D9PT )

            WRITE(*,*) 'Computation: 9 pt difference stencil on a 2D grid (STENCIL_2D9PT)'

         CASE ( STENCIL_3D7PT )

            WRITE(*,*) 'Computation: 7 pt difference stencil on a 3D grid (STENCIL_3D7PT)'

         CASE ( STENCIL_3D27PT )

            WRITE(*,*) 'Computation: 27 pt difference stencil on a 3D grid stencil (STENCIL_3D27PT)'

         CASE DEFAULT

            WRITE(*,*) '** Warning ** Unknown computation'

      END SELECT

      WRITE(*,*)
      WRITE(*,101) NX * NPX, NY * NPY, NZ * NPZ
      WRITE(*,102) NX, NY, NZ
      WRITE(*,*)
      WRITE(*,103) NUM_VARS
      WRITE(*,*)
      IF ( CHECK_DIFFUSION ) THEN
         WRITE(*,*) ' Checking diffusion each time step, using grid summation'
      ELSE
         WRITE(*,104) linfo(mype+1)%NUM_SUM_GRID, PERCENT_SUM
      END IF
      WRITE(*,*)
      WRITE(*,110) NUM_TSTEPS
      WRITE(*,*)
      WRITE(*,120) NPX, NPY, NPZ
      WRITE(*,*)
      IF ( SCALING == SCALING_STRONG ) THEN   ! Not that it really matters.
         WRITE(*,*) 'MPI version, strong scaling'
      ELSE
         WRITE(*,*) 'MPI version, weak scaling'
      END IF
      WRITE(*,*)
      IF ( NUMPES == 1 ) THEN
         WRITE(*,121) TEST_MACHINE, TEST_TIME, TEST_DATE
      ELSE
         WRITE(*,122) NUMPES, TEST_MACHINE, TEST_TIME, TEST_DATE
      END IF

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

   END SUBROUTINE PRINT_HEADER

!  ===================================================================================

   SUBROUTINE GRID_INIT ( IERR, mype )

      INTEGER, INTENT(OUT) ::  &
         IERR, mype                    ! Return status.



      ! ------------------
      ! Local Declarations
      ! ------------------

      LOGICAL ::               &
         MY_SPIKE

      INTEGER ::  &
         SPIKE_LOC_X,          & !
         SPIKE_LOC_Y,          & !
         SPIKE_LOC_Z

      ! ---------------------
      ! Executable Statements
      ! ---------------------
	WRITE(*,*) 'mini_ghost(',linfo(mype+1)%MYPE,'): grid_init()'
      IERR = 0

      ! Determine global indices (excluding ghost)

      linfo(mype+1)%MY_GLOBAL_NX_START = NX * linfo(mype+1)%MYPX + 1
      linfo(mype+1)%MY_GLOBAL_NY_START = NY * linfo(mype+1)%MYPY + 1
      linfo(mype+1)%MY_GLOBAL_NZ_START = NZ * linfo(mype+1)%MYPZ + 1

      linfo(mype+1)%MY_GLOBAL_NX_END   = linfo(mype+1)%MY_GLOBAL_NX_START + NX - 1
      linfo(mype+1)%MY_GLOBAL_NY_END   = linfo(mype+1)%MY_GLOBAL_NY_START + NY - 1
      linfo(mype+1)%MY_GLOBAL_NZ_END   = linfo(mype+1)%MY_GLOBAL_NZ_START + NZ - 1

      MY_SPIKE = .FALSE.
      IF ( CHECK_DIFFUSION ) THEN
         IF ( ( linfo(mype+1)%MY_GLOBAL_NX_START <= linfo(mype+1)%SPIKE_LOC ( 1,1 ) .AND. &
                linfo(mype+1)%SPIKE_LOC ( 1,1 ) <= linfo(mype+1)%MY_GLOBAL_NX_END ) .AND.  &
              ( linfo(mype+1)%MY_GLOBAL_NY_START <= linfo(mype+1)%SPIKE_LOC ( 2,1 ) .AND. &
                linfo(mype+1)%SPIKE_LOC ( 2,1 ) <= linfo(mype+1)%MY_GLOBAL_NY_END ) .AND.  &
              ( linfo(mype+1)%MY_GLOBAL_NZ_START <= linfo(mype+1)%SPIKE_LOC ( 3,1 ) .AND. &
               linfo(mype+1)%SPIKE_LOC ( 3,1 ) <= linfo(mype+1)%MY_GLOBAL_NZ_END ) ) THEN
            MY_SPIKE = .TRUE.
            SPIKE_LOC_X = linfo(mype+1)%SPIKE_LOC ( 1,1 ) / NPX
            SPIKE_LOC_Y = linfo(mype+1)%SPIKE_LOC ( 2,1 ) / NPY
            SPIKE_LOC_Z = linfo(mype+1)%SPIKE_LOC ( 3,1 ) / NPZ
         END IF
      END IF

       IF ( NUM_VARS > 0 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID1( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID1 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(1,1))
      END IF

      IF ( NUM_VARS > 1 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID2( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID2 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(2,1))
      END IF

      IF ( NUM_VARS > 2 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID3( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID3 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(3,1))
      END IF

      IF ( NUM_VARS > 3 ) THEN
       ! ALLOCATE ( winfo(mype+1)%GRID4( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID4 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(4,1))
      END IF

      IF ( NUM_VARS > 4 ) THEN
      !   ALLOCATE ( winfo(mype+1)%GRID5( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID5 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(5,1))
      END IF

      IF ( NUM_VARS > 5 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID6( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID6 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(6,1))
      END IF

      IF ( NUM_VARS > 6 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID7( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID7 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(7,1))
      END IF

      IF ( NUM_VARS > 7 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID8( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID8 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(8,1))
      END IF

      IF ( NUM_VARS > 8 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID9( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID9 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(9,1))
      END IF

      IF ( NUM_VARS > 9 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID10( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID10 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(10,1))
      END IF


      IF ( NUM_VARS > 10 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID11( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID11 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(11,1))
      END IF

      IF ( NUM_VARS > 11 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID12( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID12 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(12,1))
      END IF

      IF ( NUM_VARS > 12 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID13( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID13 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(13,1))
      END IF

      IF ( NUM_VARS > 13 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID14( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID14 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(14,1))
      END IF

      IF ( NUM_VARS > 14 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID15( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID15 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(15,1))
      END IF

      IF ( NUM_VARS > 15 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID16( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID16 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(16,1))
      END IF

      IF ( NUM_VARS > 16 ) THEN
      !   ALLOCATE ( winfo(mype+1)%GRID17( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID17 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(17,1))
      END IF

      IF ( NUM_VARS > 17 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID18( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID18 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(18,1))
      END IF

      IF ( NUM_VARS > 18 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID19( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID19 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(19,1))
      END IF

      IF ( NUM_VARS > 19 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID20( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID20 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(20,1))
      END IF

      IF ( NUM_VARS > 20 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID21( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID21 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(21,1))
      END IF

      IF ( NUM_VARS > 21 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID22( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID22 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(22,1))
      END IF

      IF ( NUM_VARS > 22 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID23( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID23 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(23,1))
      END IF

      IF ( NUM_VARS > 23 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID24( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID24 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(24,1))
      END IF

      IF ( NUM_VARS > 24 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID25( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID25 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(25,1))
      END IF

      IF ( NUM_VARS > 25 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID26( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID26 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(26,1))
      END IF

      IF ( NUM_VARS > 26 ) THEN
        !    ALLOCATE ( winfo(mype+1)%GRID27( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID27 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(27,1))
      END IF

      IF ( NUM_VARS > 27 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID28( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID28 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(28,1))
      END IF

      IF ( NUM_VARS > 28 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID29( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID29 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(29,1))
      END IF

      IF ( NUM_VARS > 29 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID30( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID30 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(30,1))
      END IF


      IF ( NUM_VARS > 30 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID31( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID31 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(31,1))
      END IF

      IF ( NUM_VARS > 31 ) THEN
      !   ALLOCATE ( winfo(mype+1)%GRID32( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID32 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(32,1))
      END IF

      IF ( NUM_VARS > 32 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID33( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID33 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(33,1))
      END IF

      IF ( NUM_VARS > 33 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID34( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID34 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(34,1))
      END IF

      IF ( NUM_VARS > 34 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID35( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID35 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(35,1))
      END IF

      IF ( NUM_VARS > 35 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID36( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID36 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(36,1))
      END IF

      IF ( NUM_VARS > 36 ) THEN
        ! ALLOCATE ( winfo(mype+1)%GRID37( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID37 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(37,1))
      END IF

      IF ( NUM_VARS > 37 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID38( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID38 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(38,1))
      END IF

      IF ( NUM_VARS > 38 ) THEN
       !  ALLOCATE ( winfo(mype+1)%GRID39( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID39 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(39,1))
      END IF

      IF ( NUM_VARS > 39 ) THEN   ! This is needed for winfo(mype+1)%GRID1 workspace.
       !  ALLOCATE ( winfo(mype+1)%GRID40( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( winfo(mype+1)%GRID40 )', (NX+2)*(NY+2)*(NZ+2), mype )
         CALL INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, linfo(mype+1)%SPIKES(40,1))
      END IF

      IF ( NUM_VARS > 40 ) THEN
         IERR = -1
         CALL CHECK_ERROR ( IERR, 'GRID_INIT: TOO MANY VARS', NUM_VARS, mype )
      END IF

      ALLOCATE ( winfo(mype+1)%WORK( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
      CALL CHECK_ERROR ( IERR, 'GRID_INIT: ALLOCATE ( WORK )', (NX+2)*(NY+2)*(NZ+2), mype )

      RETURN

   END SUBROUTINE GRID_INIT

!  ===================================================================================

   SUBROUTINE GRID_DEALLOC ( IERR, mype )
   
      USE MG_CONSTANTS_MOD
   
      IMPLICIT NONE
   
      ! Variable Declarations
   
      INTEGER :: &
         IERR, mype                            ! Return status
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0
   
      IF ( ALLOCATED ( winfo(mype+1)%GRID1 ) )  DEALLOCATE ( winfo(mype+1)%GRID1 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID2 ) )  DEALLOCATE ( winfo(mype+1)%GRID2 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID3 ) )  DEALLOCATE ( winfo(mype+1)%GRID3 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID4 ) )  DEALLOCATE ( winfo(mype+1)%GRID4 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID5 ) )  DEALLOCATE ( winfo(mype+1)%GRID5 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID6 ) )  DEALLOCATE ( winfo(mype+1)%GRID6 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID7 ) )  DEALLOCATE ( winfo(mype+1)%GRID7 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID8 ) )  DEALLOCATE ( winfo(mype+1)%GRID8 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID9 ) )  DEALLOCATE ( winfo(mype+1)%GRID9 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID10 ) ) DEALLOCATE ( winfo(mype+1)%GRID10 )
   
      IF ( ALLOCATED ( winfo(mype+1)%GRID11 ) ) DEALLOCATE ( winfo(mype+1)%GRID11 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID12 ) ) DEALLOCATE ( winfo(mype+1)%GRID12 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID13 ) ) DEALLOCATE ( winfo(mype+1)%GRID13 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID14 ) ) DEALLOCATE ( winfo(mype+1)%GRID14 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID15 ) ) DEALLOCATE ( winfo(mype+1)%GRID15 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID16 ) ) DEALLOCATE ( winfo(mype+1)%GRID16 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID17 ) ) DEALLOCATE ( winfo(mype+1)%GRID17 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID18 ) ) DEALLOCATE ( winfo(mype+1)%GRID18 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID19 ) ) DEALLOCATE ( winfo(mype+1)%GRID19 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID20 ) ) DEALLOCATE ( winfo(mype+1)%GRID20 )
   
      IF ( ALLOCATED ( winfo(mype+1)%GRID21 ) ) DEALLOCATE ( winfo(mype+1)%GRID21 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID22 ) ) DEALLOCATE ( winfo(mype+1)%GRID22 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID23 ) ) DEALLOCATE ( winfo(mype+1)%GRID23 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID24 ) ) DEALLOCATE ( winfo(mype+1)%GRID24 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID25 ) ) DEALLOCATE ( winfo(mype+1)%GRID25 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID26 ) ) DEALLOCATE ( winfo(mype+1)%GRID26 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID27 ) ) DEALLOCATE ( winfo(mype+1)%GRID27 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID28 ) ) DEALLOCATE ( winfo(mype+1)%GRID28 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID29 ) ) DEALLOCATE ( winfo(mype+1)%GRID29 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID30 ) ) DEALLOCATE ( winfo(mype+1)%GRID30 )
   
      IF ( ALLOCATED ( winfo(mype+1)%GRID31 ) ) DEALLOCATE ( winfo(mype+1)%GRID31 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID32 ) ) DEALLOCATE ( winfo(mype+1)%GRID32 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID33 ) ) DEALLOCATE ( winfo(mype+1)%GRID33 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID34 ) ) DEALLOCATE ( winfo(mype+1)%GRID34 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID35 ) ) DEALLOCATE ( winfo(mype+1)%GRID35 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID36 ) ) DEALLOCATE ( winfo(mype+1)%GRID36 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID37 ) ) DEALLOCATE ( winfo(mype+1)%GRID37 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID38 ) ) DEALLOCATE ( winfo(mype+1)%GRID38 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID39 ) ) DEALLOCATE ( winfo(mype+1)%GRID39 )
      IF ( ALLOCATED ( winfo(mype+1)%GRID40 ) ) DEALLOCATE ( winfo(mype+1)%GRID40 )
   
   END SUBROUTINE GRID_DEALLOC

!  ===================================================================================

   SUBROUTINE CHECK_ERROR ( IERR, ERROR_MSG, INFO, mype )

      USE MG_CONSTANTS_MOD

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) ::               &
         ERROR_MSG


      INTEGER, INTENT(IN) ::          &
         IERR,                 & ! Error code.
         INFO, mype

      CHARACTER*(MPI_MAX_ERROR_STRING) STRING
      INTEGER RESULTLEN, IERROR

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IF ( IERR /= 0 ) THEN
         WRITE(*,90) linfo(mype+1)%MYPE, TRIM(ERROR_MSG), IERR, INFO
         call MPI_ERROR_STRING(IERR, STRING, RESULTLEN, IERROR)
         WRITE(*,91) STRING
         CALL MPI_ABORT ( linfo(mype+1)%MPI_COMM_MG, -1, IERR )
      END IF

 90   FORMAT ( '** Error ** [pe ', I5, '] ', A40, '; CODE = ', I7, &
               '. Additional info:', I4 )
 91   FORMAT ( 'MPI error message: ', A80)

   END SUBROUTINE CHECK_ERROR

!  ===================================================================================

   SUBROUTINE TERMINATE ( IERR )

      USE MG_CONSTANTS_MOD

      IMPLICIT NONE

      INTEGER ::          &
         IERR                    ! Return status.

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      !CALL PROFILE_FINALIZE ( IERR )

      RETURN

   END SUBROUTINE TERMINATE

!  ===================================================================================

   SUBROUTINE INIT_GRID( MY_SPIKE, SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z, SPIKE)

      IMPLICIT NONE

      LOGICAL, INTENT(IN) :: MY_SPIKE

      INTEGER, INTENT(IN) :: SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z

      REAL(KIND=MG_REAL), INTENT(IN) :: SPIKE

    !  REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(OUT) :: &
     !    GRID

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER :: I, J, K

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IF (CHECK_DIFFUSION) THEN

!     Next lines instead of  GRID? = 0.0  to allow first touch for OpenMP

         DO K = 0, NZ+1
            DO J = 0, NY+1
               DO I = 0, NX+1
                !  GRID(I, J, K) = 0.0
               END DO
            END DO
         END DO
         IF (MY_SPIKE) THEN
           ! GRID(SPIKE_LOC_X, SPIKE_LOC_Y, SPIKE_LOC_Z) = SPIKE
         END IF

      ELSE

         DO K = 0, NZ+1
            DO J = 0, NY+1
               DO I = 0, NX+1
                !  CALL RANDOM_NUMBER(GRID(I, J, K))
               END DO
            END DO
         END DO

      END IF

      RETURN

   END SUBROUTINE INIT_GRID

!  ===================================================================================
END MODULE MG_UTILS_MOD
