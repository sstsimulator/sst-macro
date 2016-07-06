
#include <sstmac/replacements/mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

namespace redscat3 {

/* Limit the number of error reports */
#define MAX_ERRORS 10

int redscat3( int argc, char **argv )
{
    int      err = 0;
    int      *sendbuf, *recvbuf, *recvcounts;
    int      size, rank, i, j, idx, mycount, sumval;
    MPI_Comm comm;


    MTest_Init( &argc, &argv );
    comm = MPI_COMM_WORLD;

    MPI_Comm_size( comm, &size );
    MPI_Comm_rank( comm, &rank );
    recvcounts = (int *)malloc( size * sizeof(int) );
    if (!recvcounts) {
	fprintf( stderr, "Could not allocate %d ints for recvcounts\n", 
		 size );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    mycount = (1024 * 1024) / size;
    for (i=0; i<size; i++) 
	recvcounts[i] = mycount;
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
    for (i=0; i<mycount; i++) {
	recvbuf[i] = -1;
    }

    MPI_Reduce_scatter( sendbuf, recvbuf, recvcounts, MPI_INT, MPI_SUM, comm );

    sumval = size * rank + ((size - 1) * size)/2;
    /* recvbuf should be size * (rank + i) */
    for (i=0; i<mycount; i++) {
	if (recvbuf[i] != sumval) {
	    err++;
	    if (err < MAX_ERRORS) {
		fprintf( stdout, "Did not get expected value for reduce scatter\n" );
		fprintf( stdout, "[%d] Got recvbuf[%d] = %d expected %d\n",
			 rank, i, recvbuf[i], sumval );
	    }
	}
    }

    MPI_Reduce_scatter( MPI_IN_PLACE, sendbuf, recvcounts, MPI_INT, MPI_SUM, 
			comm );

    sumval = size * rank + ((size - 1) * size)/2;
    /* recv'ed values for my process should be size * (rank + i) */
    for (i=0; i<mycount; i++) {
	if (sendbuf[i] != sumval) {
	    err++;
	    if (err < MAX_ERRORS) {
		fprintf( stdout, "Did not get expected value for reduce scatter (in place)\n" );
		fprintf( stdout, "[%d] Got buf[%d] = %d expected %d\n", 
			 rank, i, sendbuf[rank*mycount+i], sumval );
	    }
	}
    }

    free(sendbuf);
    free(recvbuf);
    free(recvcounts);
       
    MTest_Finalize( err );

    MPI_Finalize( );

    return 0;
}

}
