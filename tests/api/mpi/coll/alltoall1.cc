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
#include "mpitest.h"
#include <stdlib.h>

namespace alltoall1 {

/**
static char MTEST_Descrip[] = "";
*/

int alltoall1( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size;
    int minsize = 2, count; 
    MPI_Comm      comm;
    int *sendbuf, *recvbuf, *p;
    int sendcount, recvcount;
    int i, j;
    MPI_Datatype sendtype, recvtype;

    MTest_Init( &argc, &argv );

    /** The following illustrates the use of the routines to 
       run through a selection of communicators and datatypes.
       Use subsets of these for tests that do not involve combinations 
       of communicators, datatypes, and counts of datatypes */
    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;

	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );
	
	/** printf( "Size of comm = %d\n", size ); */
  for (count = 1; count < 65000; count = count * 2) {
	    
	    /** Create a send buf and a receive buf suitable for testing
	       all to all.  */
	    sendcount = count;
	    recvcount = count;
	    sendbuf   = (int *)malloc( count * size * sizeof(int) );
	    recvbuf   = (int *)malloc( count * size * sizeof(int) );
	    sendtype  = MPI_INT;
	    recvtype  = MPI_INT;

	    if (!sendbuf || !recvbuf) {
		errs++;
		fprintf( stderr, "Failed to allocate sendbuf and/or recvbuf\n" );
		MPI_Abort( MPI_COMM_WORLD, 1 );
	    }
	    for (i=0; i<count*size; i++) 
		recvbuf[i] = -1;
	    p = sendbuf;
	    for (j=0; j<size; j++) {
		for (i=0; i<count; i++) {
		    *p++ = j * size + rank + i;
		}
	    }

	    MPI_Alltoall( sendbuf, sendcount, sendtype,
			  recvbuf, recvcount, recvtype, comm );

	    p = recvbuf;
	    for (j=0; j<size; j++) {
		for (i=0; i<count; i++) {
		    if (*p != rank * size + j + i) {
			errs++;
			if (errs < 10) {
			    fprintf( stderr, "Error with communicator %s and size=%d count=%d\n",
				     MTestGetIntracommName(), size, count );
          fprintf( stderr, "recvbuf[%d,%d] = %d, should be %d\n",
				     j,i, *p, rank * size + j + i );
			}
		    }
		    p++;
		}
	    }

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
            /** check MPI_IN_PLACE, added in MPI-2.2 */
            p = recvbuf;
            for (j=0; j<size; j++) {
                for (i=0; i<count; i++) {
                    *p++ = j * size + rank + i;
                }
            }
            MPI_Alltoall( MPI_IN_PLACE, -1/**ignored*/, MPI_DATATYPE_NULL/**ignored*/,
                          recvbuf, recvcount, recvtype, comm );
            p = recvbuf;
            for (j=0; j<size; j++) {
                for (i=0; i<count; i++) {
                    if (*p != rank * size + j + i) {
                        errs++;
                        if (errs < 10) {
                            fprintf( stderr, "Error (MPI_IN_PLACE) with communicator %s and size=%d count=%d\n",
                                     MTestGetIntracommName(), size, count );
                            fprintf(stderr, "recvbuf[%d,%d] = %d, should be %d\n",
                                    j,i, *p, rank * size + j + i );
                        }
                    }
                    p++;
                }
            }
#endif

	    free( recvbuf );
	    free( sendbuf );
	}
	MTestFreeComm( &comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}