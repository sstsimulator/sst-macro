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
#include <sstmac/compute.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"


namespace bsendpending {
/**
static char MTEST_Descrip[] = "Test the handling of BSend operations when a detach occurs before the bsend data has been sent.";
*/

int bsendpending( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest;
    unsigned char *buf, *bufp;
    int minsize = 2; 
    int i, msgsize, bufsize, outsize;
    unsigned char *msg1, *msg2, *msg3;
    MPI_Comm      comm;
    MPI_Status    status1, status2, status3;

    MTest_Init( &argc, &argv );

    /** The following illustrates the use of the routines to 
       run through a selection of communicators and datatypes.
       Use subsets of these for tests that do not involve combinations 
       of communicators, datatypes, and counts of datatypes */
    msgsize = 128 * 1024;
    msg1 = (unsigned char *)malloc( 3 * msgsize );
    msg2 = msg1 + msgsize;
    msg3 = msg2 + msgsize;
    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;
	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );
	source = 0;
	dest   = size - 1;

	/** Here is the test:  The sender */
	if (rank == source) {
	    /** Get a bsend buffer.  Make it large enough that the Bsend
	       internals will (probably) not use a eager send for the data.
	       Have three such messages */
	    bufsize = 3 * (MPI_BSEND_OVERHEAD + msgsize);
	    buf     = (unsigned char *)malloc( bufsize );
	    if (!buf) {
		fprintf( stderr, "Unable to allocate a buffer of %d bytes\n",
			 bufsize );
		MPI_Abort( MPI_COMM_WORLD, 1 );
	    }
	    
	    MPI_Buffer_attach( buf, bufsize );

	    /** Initialize the buffers */
	    for (i=0; i<msgsize; i++) {
		msg1[i] = 0xff ^ (i & 0xff);
		msg2[i] = 0xff ^ (3*i & 0xff);
		msg3[i] = 0xff ^ (5*i & 0xff);
	    }

	    /** Initiate the bsends */
	    MPI_Bsend( msg1, msgsize, MPI_CHAR, dest, 0, comm );
	    MPI_Bsend( msg2, msgsize, MPI_CHAR, dest, 0, comm );
	    MPI_Bsend( msg3, msgsize, MPI_CHAR, dest, 0, comm );

	    /** Synchronize with our partner */
	    MPI_Sendrecv( 0, 0, MPI_CHAR, dest, 10, 
			  0, 0, MPI_CHAR, dest, 10, comm, MPI_STATUS_IGNORE );

	    /** Detach the buffers.  There should be pending operations */
	    MPI_Buffer_detach ( &bufp, &outsize );
	    if (bufp != buf) {
		fprintf( stderr, "Wrong buffer returned\n" );
		errs++;
	    }
	    if (outsize != bufsize) {
		fprintf( stderr, "Wrong buffer size returned\n" );
		errs++;
	    }
	}
	else if (rank == dest) {
	    double tstart;

	    /** Clear the message buffers */
	    for (i=0; i<msgsize; i++) {
		msg1[i] = 0;
		msg2[i] = 0;
		msg3[i] = 0;
	    }

	    /** Wait for the synchronize */
	    MPI_Sendrecv( 0, 0, MPI_CHAR, source, 10, 
			  0, 0, MPI_CHAR, source, 10, comm, MPI_STATUS_IGNORE );

	    /** Wait 2 seconds */
	    SSTMAC_compute(2);

	    /** Now receive the messages */
	    MPI_Recv( msg1, msgsize, MPI_CHAR, source, 0, comm, &status1 );
	    MPI_Recv( msg2, msgsize, MPI_CHAR, source, 0, comm, &status2 );
	    MPI_Recv( msg3, msgsize, MPI_CHAR, source, 0, comm, &status3 );

	    /** Check that we have the correct data */
	    for (i=0; i<msgsize; i++) {
		if (msg1[i] != (0xff ^ (i & 0xff))) { 
		    if (errs < 10) {
			fprintf( stderr, "msg1[%d] = %d\n", i, msg1[i] );
		    }
		    errs++;
		}
		if (msg2[i] != (0xff ^ (3*i & 0xff))) {
		    if (errs < 10) {
			fprintf( stderr, "msg2[%d] = %d\n", i, msg2[i] );
		    }
		    errs++;
		}
		if (msg3[i] != (0xff ^ (5*i & 0xff))) {
		    if (errs < 10) {
			fprintf( stderr, "msg2[%d] = %d\n", i, msg2[i] );
		    }
		    errs++;
		}
	    }
	    
	}
		
	
	MTestFreeComm( &comm );
    }
    free( msg1 );

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}