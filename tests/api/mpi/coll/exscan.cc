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
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

namespace exscan {
/**
static char MTEST_Descrip[] = "Test MPI_Exscan";
*/

int exscan( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size;
    int minsize = 2, count; 
    int *sendbuf, *recvbuf, i;
    MPI_Comm      comm;

    MTest_Init( &argc, &argv );

    /** The following illustrates the use of the routines to 
       run through a selection of communicators and datatypes.
       Use subsets of these for tests that do not involve combinations 
       of communicators, datatypes, and counts of datatypes */
    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;

	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );
	
	for (count = 1; count < 65000; count = count * 2) {

	    sendbuf = (int *)malloc( count * sizeof(int) );
	    recvbuf = (int *)malloc( count * sizeof(int) );

	    for (i=0; i<count; i++) {
		sendbuf[i] = rank + i * size;
		recvbuf[i] = -1;
	    }
	    
	    MPI_Exscan( sendbuf, recvbuf, count, MPI_INT, MPI_SUM, comm );

	    /** Check the results.  rank 0 has no data */
	    if (rank > 0) {
		int result;
		for (i=0; i<count; i++) {
		    result = rank * i * size + ((rank) * (rank-1))/2;
		    if (recvbuf[i] != result) {
			errs++;
			if (errs < 10) {
			    fprintf( stderr, "Error in recvbuf[%d] = %d on %d, expected %d\n",
				     i, recvbuf[i], rank, result );
			}
		    }
		}
	    }

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
            /** now try the MPI_IN_PLACE flavor */
            for (i=0; i<count; i++) {
                sendbuf[i] = -1; /** unused */
                recvbuf[i] = rank + i * size;
            }

            MPI_Exscan( MPI_IN_PLACE, recvbuf, count, MPI_INT, MPI_SUM, comm );

            /** Check the results.  rank 0's data must remain unchanged */
            for (i=0; i<count; i++) {
                int result;
                if (rank == 0)
                    result = rank + i * size;
                else
                    result = rank * i * size + ((rank) * (rank-1))/2;
                if (recvbuf[i] != result) {
                    errs++;
                    if (errs < 10) {
                        fprintf( stderr, "Error in recvbuf[%d] = %d on %d, expected %d\n",
                                 i, recvbuf[i], rank, result );
                    }
                }
            }
#endif

	    free( sendbuf );
	    free( recvbuf );
	}
	MTestFreeComm( &comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}