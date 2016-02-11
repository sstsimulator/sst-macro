! uncomment for real mpi program
!program integrator

! uncomment for simulator
subroutine user_skeleton_main()

  !use mpi
  include "sstmac/libraries/mpi/sstmac_mpi_f.h"
  include "sstmac/software/libraries/compute/sstmac_compute_f.h"

  integer i, world, ierr, rank, size
  integer * 8 nintervals, nloop
  double precision start, end, x, fx, interval_size, area, total, time
  integer fakebuf

  nintervals = 10000
  start = 0
  end = 1
  area=0;
  total=0;

  call mpi_init(ierr)

  ! uncomment for real mpi program
  world = MPI_COMM_WORLD
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,size,ierr)

  if(nintervals < 1) then
    stop 1
  end if

! uncomment for real mpi program
!  interval_size = (end - start) / nintervals;
!  do 10 i=rank, nintervals, size
!    x = (i + 0.5) * interval_size
!    area = area + fx(x) * interval_size
!10 continue

! uncomment for simulator
! pretend computation takes 0.1us per loop iteration
  nloop = nintervals/size
  if(rank < MOD(nintervals,size) ) then
    nloop = nloop + 1 
  end if
  time = nloop * 1e-7
  call sstmac_compute(time)

! uncomment for real mpi program
!  call mpi_reduce(area,total,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,ierr)

! uncomment for simulator
  call mpi_reduce(fakebuf,fakebuf,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,world,ierr)

  if(rank .EQ. 0) then
    write (*,100) 'result is ', total
100 format (A,F8.6)
  end if

  call mpi_finalize(ierr);

end


!--------------------------------
!  function to integrate
!--------------------------------

double precision function fx(x)
  double precision x
  fx = x * x
  return
end
