
#include <sstmac/replacements/mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

namespace redscatblk3{

int redscatblk3( int argc, char **argv )
{
    int      err = 0;
    int      *sendbuf, *recvbuf;
    int      size, rank, i, j, idx, mycount, sumval;
    MPI_Comm comm;


    MTest_Init( &argc, &argv );
    comm = MPI_COMM_WORLD;

    MPI_Comm_size( comm, &size );
    MPI_Comm_rank( comm, &rank );
    mycount = (1024 * 1024) / size;

    sendbuf = (int *) malloc( mycount * size * sizeof(int) );
    if (!sendbuf) {
	fprintf( stderr, "Could not allocate %d ints for sendbuf\n", 
		 mycount * size );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    idx = 0;
    for (i=0; i<size; i++) {
	for (j=0; j<mycount; j++) {
	    sendbuf[idx++] = rank + i;
	}
    }
    recvbuf = (int *)malloc( mycount * sizeof(int) );
    if (!recvbuf) {
	fprintf( stderr, "Could not allocate %d ints for recvbuf\n", 
		 mycount );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    MPI_Reduce_scatter_block( sendbuf, recvbuf, mycount, MPI_INT, MPI_SUM, 
			      comm );

    sumval = size * rank + ((size - 1) * size)/2;
    /* recvbuf should be size * (rank + i) */
    for (i=0; i<mycount; i++) {
	if (recvbuf[i] != sumval) {
	    err++;
	    fprintf( stdout, "Did not get expected value for reduce scatter\n" );
	    fprintf( stdout, "[%d] Got %d expected %d\n", rank, recvbuf[i], sumval );
	}
    }

    MPI_Reduce_scatter_block( MPI_IN_PLACE, sendbuf, mycount, MPI_INT, MPI_SUM, 
			comm );

    sumval = size * rank + ((size - 1) * size)/2;
    /* recv'ed values for my process should be size * (rank + i) */
    for (i=0; i<mycount; i++) {
	if (sendbuf[rank*mycount+i] != sumval) {
	    err++;
	    fprintf( stdout, "Did not get expected value for reduce scatter (in place)\n" );
	    fprintf( stdout, "[%d] Got %d expected %d\n", rank, sendbuf[rank*mycount+i], sumval );
	}
    }

    free(sendbuf);
    free(recvbuf);
       
    MTest_Finalize( err );

    MPI_Finalize( );

    return 0;
}

}
