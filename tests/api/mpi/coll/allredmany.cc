
#include <stdio.h>
#include <sstmac/replacements/mpi.h>

namespace allredmany {

/*
 * This example should be run with 2 processes and tests the ability of the
 * implementation to handle a flood of one-way messages.
 */

int allredmany( int argc, char **argv )
{
  double wscale = 10.0, scale;
  int numprocs, myid,i;

  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD,&myid);

  for ( i=0; i<256; i++) {
    MPI_Allreduce(&wscale,&scale,1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD);
  }

  if (myid == 0) {
      /* If we get here at all, we're ok */
      printf( " No Errors\n" );
  }
  MPI_Finalize();
  
  return 0;
}

}
