

#include <sstmac/replacements/mpi.h>

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
