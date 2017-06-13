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

namespace anyall {
#define MAX_MSGS 30

/**
static char MTEST_Descrip[] = "One implementation delivered incorrect data when an MPI recieve uses both ANY_SOURCE and ANY_TAG";
*/

int anyall( int argc, char *argv[] )
{
    int         wrank, wsize, master, worker, i, j, idx, count;
    int         errs = 0;
    MPI_Request r[MAX_MSGS];
    int         buf[MAX_MSGS][MAX_MSGS];
    MPI_Comm    comm;
    MPI_Status  status;

    MTest_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD, &wrank );
    MPI_Comm_size( MPI_COMM_WORLD, &wsize );

    comm = MPI_COMM_WORLD;
    master = 0;
    worker = 1;

    /** The test takes advantage of the ordering rules for messages*/

    if (wrank == master) {
	/** Initialize the send buffer */
	for (i=0; i<MAX_MSGS; i++) {
	    for (j=0; j<MAX_MSGS; j++) {
		buf[i][j] = i*MAX_MSGS + j;
	    }
	}
	MPI_Barrier( MPI_COMM_WORLD );
	for (i=0; i<MAX_MSGS; i++) {
	    MPI_Send( buf[i], MAX_MSGS-i, MPI_INT, worker, 3, comm );
	}
    }
    else if (wrank == worker) {
	/** Initialize the recv buffer */
	for (i=0; i<MAX_MSGS; i++) {
	    for (j=0; j<MAX_MSGS; j++) {
		buf[i][j] = -1;
	    }
	}
	for (i=0; i<MAX_MSGS; i++) {
	    MPI_Irecv( buf[i], MAX_MSGS-i, MPI_INT, MPI_ANY_SOURCE, 
		       MPI_ANY_TAG, comm, &r[i] );
	}
	MPI_Barrier( MPI_COMM_WORLD );
	for (i=0; i<MAX_MSGS; i++) {
	    MPI_Waitany( MAX_MSGS, r, &idx, &status );
	    /** Message idx should have length MAX_MSGS-idx */
	    MPI_Get_count( &status, MPI_INT, &count );
	    if (count != MAX_MSGS-idx) {
		errs++;
	    }
	    else {
		/** Check for the correct answers */
		for (j=0; j < MAX_MSGS-idx; j++) {
		    if (buf[idx][j] != idx * MAX_MSGS + j) {
			errs ++;
			printf( "Message %d [%d] is %d, should be %d\n",
				idx, j, buf[idx][j], idx * MAX_MSGS + j );
		    }
		}
	    }
	}
    }
    else {
	MPI_Barrier( MPI_COMM_WORLD );
    }
	
    MTest_Finalize( errs );
    MPI_Finalize();

    return 0;
}
}