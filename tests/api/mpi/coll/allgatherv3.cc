/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/replacements/mpi.h>
#include "mpitest.h"
#include <stdlib.h>
#include <stdio.h>

namespace allgatherv3 {
/** Gather data from a vector to contiguous.  This is 
   the trivial version based on the allgather test (allgatherv but with
   constant data sizes) */

int allgatherv3( int argc, char **argv )
{
    double *vecout, *invec;
    MPI_Comm comm;
    int    count, minsize = 2;
    int    i, errs = 0;
    int    rank, size;
    int    *displs, *recvcounts;

    MTest_Init( &argc, &argv );

    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;
	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );

	displs     = (int *)malloc( size * sizeof(int) );
	recvcounts = (int *)malloc( size * sizeof(int) );
	
        for (count = 1; count < 9000; count = count * 2) {
	    invec = (double *)malloc( count * sizeof(double) );
            vecout = (double *)malloc( size * count * sizeof(double) );
            
            for (i=0; i<count; i++) {
                invec[i] = rank*count+i;
            }
            for (i=0; i<size; i++) {
                recvcounts[i] = count;
                displs[i]    = i * count;
            }
            MPI_Allgatherv( invec, count, MPI_DOUBLE, 
                            vecout, recvcounts, displs, MPI_DOUBLE, comm );
            for (i=0; i<count*size; i++) {
                if (vecout[i] != i) {
                    errs++;
                    if (errs < 10) {
                        fprintf( stderr, "vecout[%d]=%d\n",
                                 i, (int)vecout[i] );
                    }
                }
            }
	    free( invec );
            free( vecout );
        }
	free( displs );
	free( recvcounts );
	MTestFreeComm( &comm );
    }
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}


}