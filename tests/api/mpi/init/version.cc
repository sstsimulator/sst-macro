

#include <sstmac/replacements/mpi.h>
#include <stdio.h>
#include "mpitest.h"

namespace version {
int version( int argc, char *argv[] )
{
    int errs = 0;
    int majversion, subversion;

    MTest_Init( &argc, &argv );

    MPI_Get_version( &majversion, &subversion );
    if (majversion != MPI_VERSION) {
	errs++;
	printf( "Major version is %d but is %d in the mpi.h file\n", 
		majversion, MPI_VERSION );
    }
    if (subversion != MPI_SUBVERSION) {
	errs++;
	printf( "Minor version is %d but is %d in the mpi.h file\n", 
		subversion, MPI_SUBVERSION );
    }
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}
