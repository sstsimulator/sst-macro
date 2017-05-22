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

#include <stdio.h>
#include <stdlib.h>
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace bsendfrag {
/**
static char MTEST_Descrip[] = "Test bsend message handling where \
different messages are received in different orders";
*/

/**
 * Notes on the test.
 *
 * To ensure that messages remain in the bsend buffer until received,
 * messages are sent with size MSG_SIZE (ints).  
 */

#define MSG_SIZE 17000

int bsendfrag( int argc, char *argv[] )
{
    int errs = 0;
    int b1[MSG_SIZE], b2[MSG_SIZE], b3[MSG_SIZE], b4[MSG_SIZE];
    int src, dest, size, rank, i;
    MPI_Comm comm;
    MPI_Status status;

    MTest_Init( &argc, &argv );

    MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    comm = MPI_COMM_WORLD;
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    if (size < 2) {
	errs++;
	fprintf( stderr, "At least 2 processes required\n" );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    src  = 0;
    dest = 1;

    if (rank == src) {
	int *buf, bufsize, bsize;

	bufsize = 4 * (MSG_SIZE * sizeof(int) + MPI_BSEND_OVERHEAD);
	buf = (int *)malloc( bufsize );
	if (!buf) {
	    fprintf( stderr, "Could not allocate buffer of %d bytes\n", 
		     bufsize );
	    MPI_Abort( MPI_COMM_WORLD, 1 );
	}
	MPI_Buffer_attach( buf, bufsize );

	/** Initialize data */
	for (i=0; i<MSG_SIZE; i++) {
	    b1[i] = i;
	    b2[i] = MSG_SIZE + i;
	    b3[i] = 2 * MSG_SIZE + i;
	    b4[i] = 3 * MSG_SIZE + i;
	}
	/** Send and reset buffers after bsend returns */
	MPI_Bsend( b1, MSG_SIZE, MPI_INT, dest, 0, comm );
	for (i=0; i<MSG_SIZE; i++) b1[i] = -b1[i];
	MPI_Bsend( b2, MSG_SIZE, MPI_INT, dest, 1, comm );
	for (i=0; i<MSG_SIZE; i++) b2[i] = -b2[i];
	MPI_Bsend( b3, MSG_SIZE, MPI_INT, dest, 2, comm );
	for (i=0; i<MSG_SIZE; i++) b3[i] = -b3[i];
	MPI_Bsend( b4, MSG_SIZE, MPI_INT, dest, 3, comm );
	for (i=0; i<MSG_SIZE; i++) b4[i] = -b4[i];

	MPI_Barrier( comm );
	/** Detach waits until all messages received */
	MPI_Buffer_detach( &buf, &bsize );
    }
    else if (rank == dest) {
	
	MPI_Barrier( comm );
	MPI_Recv( b2, MSG_SIZE, MPI_INT, src, 1, comm, &status );
	MPI_Recv( b1, MSG_SIZE, MPI_INT, src, 0, comm, &status );
	MPI_Recv( b4, MSG_SIZE, MPI_INT, src, 3, comm, &status );
	MPI_Recv( b3, MSG_SIZE, MPI_INT, src, 2, comm, &status );

	/** Check received data */
	for (i=0; i<MSG_SIZE; i++) {
	    if (b1[i] != i) {
		errs++;
		if (errs < 16) printf( "b1[%d] is %d\n", i, b1[i] );
	    }
	    if (b2[i] != MSG_SIZE + i) {
		errs++;
		if (errs < 16) printf( "b2[%d] is %d\n", i, b2[i] );
	    }
	    if (b3[i] != 2 * MSG_SIZE + i) {
		errs++;
		if (errs < 16) printf( "b3[%d] is %d\n", i, b3[i] );
	    }
	    if (b4[i] != 3 * MSG_SIZE + i) {
		errs++;
		if (errs < 16) printf( "b4[%d] is %d\n", i, b4[i] );
	    }
	}
    }
    else {
	MPI_Barrier( comm );
    }


    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}