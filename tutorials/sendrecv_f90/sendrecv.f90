module mymod

	type mytype
		integer int_buf
	end type mytype
end module mymod

subroutine user_skeleton_main()
!use mpi
use mymod
 implicit none
  include "sstmac/libraries/mpi/sstmac_mpi_f.h"
  integer ierr, message_size, me, nproc, tag, dst, src, stat(MPI_STATUS_SIZE), msg_size

	type(mytype) :: ibuf
	integer int_arr_buf(3)
	integer, dimension(:,:), allocatable :: int_multi_arr
	real real_buf
	real real_arr_buf(3)
	double precision doub_buf
	double precision doub_arr_buf(3)



  msg_size = 3
  tag = 0
  dst = 1
  src = 0


  allocate(int_multi_arr(msg_size, msg_size))

  call MPI_INIT(ierr)

  !This is one main difference, world comm has to be a function

  call MPI_Comm_rank(MPI_COMM_WORLD,me,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,nproc,ierr)


   if (me == 0) then
  	 ibuf%int_buf = 32
   endif

   call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if (me == 0 .and. nproc /= 2) then
      write (*,*) 'sendrecv should only be run with two processors'
      call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
  endif

!  -------- test for integer buffer
  if (me == 0) then
  	  write (*,*) 'rank ',me,' sending a message'
      call MPI_Send(ibuf%int_buf, 1, MPI_INTEGER, dst, tag, MPI_COMM_WORLD, ierr)

  else
  		! write (*,*) 'rank ',me,' before recv, i have: ',ibuf%int_buf  ! this output is machine dependent!
      call MPI_Recv(ibuf%int_buf, 1, MPI_INTEGER, src, tag, MPI_COMM_WORLD, stat, ierr)
      write (*,*) 'rank ',me,' receiving a message: ',ibuf%int_buf
  endif

! --------- test for integer array buffer
   if (me == 0) then
   int_arr_buf(1) = 127
   int_arr_buf(2) = 345
   int_arr_buf(3) = 987
      call MPI_Send(int_arr_buf, msg_size, MPI_INTEGER, dst, tag, MPI_COMM_WORLD, ierr)
      write (*,*) 'rank ',me,' sending a message'
  else
      call MPI_Recv(int_arr_buf, msg_size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, stat, ierr)
      write (*,*) 'rank ',me,' receiving a message: ',int_arr_buf(1),' ',int_arr_buf(2),' ',int_arr_buf(3)
  endif


 ! --------- test for multi-dimensional integer array buffer
 !  if (me == 0) then
 !     call MPI_Send(int_multi_arr, msg_size*msg_size, MPI_INTEGER, dst, tag, MPI_COMM_WORLD, ierr)
 !     write (*,*) 'rank ',me,' sending a message'
 ! else
 !     call MPI_Recv(int_multi_arr, msg_size*msg_size, MPI_INTEGER, src, tag, MPI_COMM_WORLD, stat, ierr)
 !     write (*,*) 'rank ',me,' receiving a message'
 ! endif


! ---------- test for real buffer
   if (me == 0) then
      call MPI_Send(real_buf, 1, MPI_REAL, dst, tag, MPI_COMM_WORLD, ierr)
      write (*,*) 'rank ',me,' sending a message'
  else
      call MPI_Recv(real_buf, 1, MPI_REAL, src, tag, MPI_COMM_WORLD, stat, ierr)
      write (*,*) 'rank ',me,' receiving a message'
  endif

! --------- test for real array buffer

   if (me == 0) then
      call MPI_Send(real_arr_buf, msg_size, MPI_REAL, dst, tag, MPI_COMM_WORLD, ierr)
      write (*,*) 'rank ',me,' sending a message'
  else
      call MPI_Recv(real_arr_buf, msg_size, MPI_REAL, src, tag, MPI_COMM_WORLD, stat, ierr)
      write (*,*) 'rank ',me,' receiving a message'
  endif

! ---------- test for double buffer
   if (me == 0) then
      call MPI_Send(doub_buf, 1, MPI_DOUBLE_PRECISION, dst, tag, MPI_COMM_WORLD, ierr)
      write (*,*) 'rank ',me,' sending a message'
  else
      call MPI_Recv(doub_buf, 1, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, stat, ierr)
      write (*,*) 'rank ',me,' receiving a message'
  endif

! ---------- test for double array buffer
   if (me == 0) then
      call MPI_Send(doub_arr_buf, msg_size, MPI_DOUBLE_PRECISION, dst, tag, MPI_COMM_WORLD, ierr)
      write (*,*) 'rank ',me,' sending a message'
  else
      call MPI_Recv(doub_arr_buf, msg_size, MPI_DOUBLE_PRECISION, src, tag, MPI_COMM_WORLD, stat, ierr)
      write (*,*) 'rank ',me,' receiving a message'
  endif

  deallocate(int_multi_arr)

  call MPI_FINALIZE(ierr)
end
