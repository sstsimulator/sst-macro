/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */


#include <sstmac/replacements/mpi/mpi.h>

namespace exitst3 {
/** 
 * This is a special test to check that mpiexec handles the death of
 * some processes without an Abort or clean exit
 */
int exitst3( int argc, char *argv[] )
{
    int rank, size;
    MPI_Init( 0, 0 );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Barrier( MPI_COMM_WORLD );
    if (rank == size-1) {
	/** Cause some processes to exit */
	int *p =0 ;
	*p = rank;
    }
    MPI_Finalize( );
    return 0;
}

}