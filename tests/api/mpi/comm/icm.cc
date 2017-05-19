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

namespace icm {
/**
static char MTEST_Descrip[] = "Test intercomm merge, including the choice of the high value";
*/

int icm( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, rsize;
    int nsize, nrank;
    int minsize = 2;
    int isLeft;
    MPI_Comm      comm, comm1, comm2, comm3, comm4;

    MTest_Init( &argc, &argv );

    /** The following illustrates the use of the routines to 
       run through a selection of communicators and datatypes.
       Use subsets of these for tests that do not involve combinations 
       of communicators, datatypes, and counts of datatypes */
    while (MTestGetIntercomm( &comm, &isLeft, minsize )) {
	if (comm == MPI_COMM_NULL) continue;
	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_remote_size( comm, &rsize );
	MPI_Comm_size( comm, &size );

	/** Try building intercomms */
	MPI_Intercomm_merge( comm, isLeft, &comm1 );
	/** Check the size and ranks */
	MPI_Comm_size( comm1, &nsize );
	MPI_Comm_rank( comm1, &nrank );
	if (nsize != size + rsize) {
	    errs++;
	    printf( "(1) Comm size is %d but should be %d\n", nsize,
		    size + rsize );
	    if (isLeft) {
		/** The left processes should be high */
		if (nrank != rsize + rank) {
		    errs++;
		    printf( "(1) rank for high process is %d should be %d\n",
			    nrank, rsize + rank );
		}
	    }
	    else {
		/** The right processes should be low */
		if (nrank != rank) {
		    errs++;
		    printf( "(1) rank for low process is %d should be %d\n",
			    nrank, rank );
		}
	    }
	}
	
 	MPI_Intercomm_merge( comm, !isLeft, &comm2 ); 
	/** Check the size and ranks */
	MPI_Comm_size( comm1, &nsize );
	MPI_Comm_rank( comm1, &nrank );
	if (nsize != size + rsize) {
	    errs++;
	    printf( "(2) Comm size is %d but should be %d\n", nsize,
		    size + rsize );
	    if (!isLeft) {
		/** The right processes should be high */
		if (nrank != rsize + rank) {
		    errs++;
		    printf( "(2) rank for high process is %d should be %d\n",
			    nrank, rsize + rank );
		}
	    }
	    else {
		/** The left processes should be low */
		if (nrank != rank) {
		    errs++;
		    printf( "(2) rank for low process is %d should be %d\n",
			    nrank, rank );
		}
	    }
	}
	

 	MPI_Intercomm_merge( comm, 0, &comm3 ); 

 	MPI_Intercomm_merge( comm, 1, &comm4 ); 
	
	MPI_Comm_free( &comm1 );
	MPI_Comm_free( &comm2 );
	MPI_Comm_free( &comm3 ); 
	MPI_Comm_free( &comm4 );
      
	MTestFreeComm( &comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}