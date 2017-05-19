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

namespace rqstatus {
/**
static char MTEST_Descrip[] = "Test Request_get_status";
*/

int rqstatus( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest;
    int buf[2], flag, count;
    MPI_Comm      comm;
    MPI_Status    status, status2;
    MPI_Request   req;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;
    /** Determine the sender and receiver */
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );
    source = 0;
    dest   = size - 1;


    /** Handling MPI_REQUEST_NULL in MPI_Request_get_status was only required
       starting with MPI-2.2. */
#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
    MPI_Request_get_status( MPI_REQUEST_NULL, &flag, &status );
    if (!flag) {
        errs++;
        fprintf( stderr, "flag not true for MPI_REQUEST_NULL, flag=%d\n", flag );
    }
    if ((status.MPI_SOURCE != MPI_ANY_SOURCE) ||
        (status.MPI_TAG != MPI_ANY_TAG) ||
        (status.MPI_ERROR != MPI_SUCCESS))
    {
        errs++;
        fprintf( stderr, "non-empty MPI_Status returned for MPI_REQUEST_NULL\n" );
    }

    /** also pass MPI_STATUS_IGNORE to make sure the implementation doesn't
     * blow up when it is passed as the status argument */
    MPI_Request_get_status( MPI_REQUEST_NULL, &flag, MPI_STATUS_IGNORE );
    if (!flag) {
        errs++;
        fprintf( stderr, "flag not true for MPI_REQUEST_NULL with MPI_STATUS_IGNORE, flag=%d\n", flag );
    }
#endif

    if (rank == source) {
	buf[0] = size;
	buf[1] = 3;
	MPI_Ssend( buf, 2, MPI_INT, dest, 10, comm );
    }
    if (rank == dest) {
	MPI_Irecv( buf, 2, MPI_INT, source, 10, comm, &req );
    }
    MPI_Barrier( comm );
    /** At this point, we know that the receive has at least started,
       because of the Ssend.  Check the status on the request */
    if (rank == dest) {
	status.MPI_SOURCE = -1;
	status.MPI_TAG    = -1;
	MPI_Request_get_status( req, &flag, &status );
	if (flag) {
	    if (status.MPI_TAG != 10) {
		errs++;
		fprintf( stderr, "Tag value %d should be 10\n", status.MPI_TAG );
	    }
	    if (status.MPI_SOURCE != source) {
		errs++;
		fprintf( stderr, "Source value %d should be %d\n", status.MPI_SOURCE, source );
	    }
	    MPI_Get_count( &status, MPI_INT, &count );
	    if (count != 2) {
		errs++;
		fprintf( stderr, "Count value %d should be 2\n", count );
	    }
	}
	else {
	    errs++;
	    fprintf( stderr, "Unexpected flag value from get_status\n" );
	}
	/** Now, complete the request */
	MPI_Wait( &req, &status2 );
	/** Check that the status is correct */
	if (status2.MPI_TAG != 10) {
	    errs++;
	    fprintf( stderr, "(wait)Tag value %d should be 10\n", status2.MPI_TAG );
	}
	if (status2.MPI_SOURCE != source) {
	    errs++;
	    fprintf( stderr, "(wait)Source value %d should be %d\n", status2.MPI_SOURCE, source );
	}
	MPI_Get_count( &status2, MPI_INT, &count );
	if (count != 2) {
	    errs++;
	    fprintf( stderr, "(wait)Count value %d should be 2\n", count );
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}