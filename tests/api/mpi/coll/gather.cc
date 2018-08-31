/*
 *
 *  (C) 2003 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */


#include <sstmac/replacements/mpi.h>
#include "mpitest.h"
#include <stdlib.h>
#include <stdio.h>
#include <sprockit/debug.h>
#include <sumi/collective_actor.h>

namespace gather {
/** Gather data from a vector to contiguous */

int gather( int argc, char **argv )
{
    MPI_Datatype vec;
    MPI_Comm     comm;
    double *vecin, *vecout;
    int    minsize = 2, count;
    int    root, i, n, stride, errs = 0;
    int    rank, size;

    MTest_Init( &argc, &argv );

    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;
	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );
	
  for (root=0; root<size; root++) {
      for (int iter = 0; iter < 1; ++iter) {
    n = 12;
		stride = 10;
    int blocklength = 1;
    //stride = blocklength;
		vecin = (double *)malloc( n * stride * size * sizeof(double) );
		vecout = (double *)malloc( size * n * sizeof(double) );
		
    MPI_Type_vector( n, blocklength, stride, MPI_DOUBLE, &vec );
		MPI_Type_commit( &vec );
		
		for (i=0; i<n*stride; i++) vecin[i] =-2;
		for (i=0; i<n; i++) vecin[i*stride] = rank * n + i;
		
    MPI_Gather( vecin, blocklength, vec, vecout, n, MPI_DOUBLE, root, comm );

		if (rank == root) {
		    for (i=0; i<n*size; i++) {
			if (vecout[i] != i) {
			    errs++;
			    if (errs < 10) {
        fprintf( stderr, "root %d rank %d vecout[%d]=%d\n",
           root, rank, i, (int)vecout[i] );
			    }
			}
		    }
		}
		MPI_Type_free( &vec );
		free( vecin );
		free( vecout );
	    }
	}
	MTestFreeComm( &comm );
    }

    /** do a zero length gather */
    MPI_Gather( NULL, 0, MPI_BYTE, NULL, 0, MPI_BYTE, 0, MPI_COMM_WORLD );

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}


}