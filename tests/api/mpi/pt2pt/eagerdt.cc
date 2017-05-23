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

namespace eagerdt {
/**
static char MTEST_Descrip[] = "Test of a large number of derived-datatype messages eagerly, with no preposted receive so that an MPI implementation may have to queue up messages on the sending side";
*/

#define MAX_MSGS 30

int eagerdt( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, dest, source;
    int i, indices[40];
    MPI_Aint extent;
    int *buf, *bufs[MAX_MSGS];
    MPI_Comm      comm;
    MPI_Datatype  dtype;
    MPI_Request   req[MAX_MSGS];

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );
    source = 0;
    dest   = size - 1;
    
    /** Setup by creating a blocked datatype that is likely to be processed
       in a piecemeal fashion */
    for (i=0; i<30; i++) {
	indices[i] = i*40;
    }

    /** 30 blocks of size 10 */
    MPI_Type_create_indexed_block( 30, 10, indices, MPI_INT, &dtype );
    MPI_Type_commit( &dtype );
    
    /** Create the corresponding message buffers */
    MPI_Type_extent( dtype, &extent );
    for (i=0; i<MAX_MSGS; i++) {
	bufs[i] = (int *)malloc( extent );
	if (!bufs[i]) {
	    fprintf( stderr, "Unable to allocate buffer %d of size %ld\n", 
		    	i, (long)extent );
	    MPI_Abort( MPI_COMM_WORLD, 1 );
	}
    }
    buf = (int *)malloc( 10 * 30 * sizeof(int) );
    
    MPI_Barrier( MPI_COMM_WORLD );
    if (rank == dest) {
	MTestSleep( 2 );
	for (i=0; i<MAX_MSGS; i++) {
	    MPI_Recv( buf, 10*30, MPI_INT, source, i, comm, 
		      MPI_STATUS_IGNORE );
	}
    }
    else if (rank == source ) {
	for (i=0; i<MAX_MSGS; i++) {
	    MPI_Isend( bufs[i], 1, dtype, dest, i, comm, &req[i] );
	}
	MPI_Waitall( MAX_MSGS, req, MPI_STATUSES_IGNORE );
    }

    MPI_Type_free( &dtype );
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}