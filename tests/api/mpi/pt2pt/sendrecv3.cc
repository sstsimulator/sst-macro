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

namespace sendrecv3 {
/**
static char MTEST_Descrip[] = "Head to head send-recv to test backoff in device when large messages are being transferred";
*/

#define  MAX_NMSGS 100
int sendrecv3( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest, partner;
    int i, testnum; 
    double tsend;
    static int msgsizes[] = { 100, 1000, 10000, 100000, -1 };
    static int nmsgs[]    = { 100, 10,   10,    4 };
    MPI_Comm      comm;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );
    source = 0;
    dest   = 1;
    if (size < 2) {
	printf( "This test requires at least 2 processes\n" );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    for (testnum=0; msgsizes[testnum] > 0; testnum++) {
	if (rank == source || rank == dest) {
	    int nmsg = nmsgs[testnum];
	    int msgSize = msgsizes[testnum];
	    MPI_Request r[MAX_NMSGS];
	    int *buf[MAX_NMSGS];

	    for (i=0; i<nmsg; i++) {
		buf[i] = (int *)malloc( msgSize );
	    }
	    partner = (rank + 1) % 2;

	    MPI_Sendrecv( MPI_BOTTOM, 0, MPI_INT, partner, 10, 
			  MPI_BOTTOM, 0, MPI_INT, partner, 10, comm, 
			  MPI_STATUS_IGNORE );
	    /** Try to fill up the outgoing message buffers */
	    for (i=0; i<nmsg; i++) {
		MPI_Isend( buf[i], msgSize, MPI_CHAR, partner, testnum, comm,
			   &r[i] );
	    }
	    for (i=0; i<nmsg; i++) {
		MPI_Recv( buf[i], msgSize, MPI_CHAR, partner, testnum, comm,
			  MPI_STATUS_IGNORE );
	    }
	    MPI_Waitall( nmsg, r, MPI_STATUSES_IGNORE );

	    /** Repeat the test, but make one of the processes sleep */
	    MPI_Sendrecv( MPI_BOTTOM, 0, MPI_INT, partner, 10, 
			  MPI_BOTTOM, 0, MPI_INT, partner, 10, comm, 
			  MPI_STATUS_IGNORE );
	    if (rank == dest) MTestSleep( 1 );
	    /** Try to fill up the outgoing message buffers */
	    tsend = MPI_Wtime();
	    for (i=0; i<nmsg; i++) {
		MPI_Isend( buf[i], msgSize, MPI_CHAR, partner, testnum, comm,
			   &r[i] );
	    }
	    tsend = MPI_Wtime() - tsend;
	    for (i=0; i<nmsg; i++) {
		MPI_Recv( buf[i], msgSize, MPI_CHAR, partner, testnum, comm,
			  MPI_STATUS_IGNORE );
	    }
	    MPI_Waitall( nmsg, r, MPI_STATUSES_IGNORE );

	    if (tsend > 0.5) {
		printf( "Isends for %d messages of size %d took too long (%f seconds)\n", nmsg, msgSize, tsend );
		errs++;
	    }
	    MTestPrintfMsg( 1, "%d Isends for size = %d took %f seconds\n", 
			    nmsg, msgSize, tsend );

	    for (i=0; i<nmsg; i++) {
		free( buf[i] );
	    }
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}