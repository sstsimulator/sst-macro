/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */


#include <sstmac/replacements/mpi/mpi.h>

namespace exitst1 {
/** 
 * This is a special test to check that mpiexec handles zero/non-zero 
 * return status from an application
 */
int exitst1( int argc, char *argv[] )
{
    MPI_Init( 0, 0 );
    MPI_Finalize( );
    return 1;
}

}
