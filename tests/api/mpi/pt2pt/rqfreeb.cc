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

namespace rqfreeb {
/** Test Ibsend and Request_free */
int rqfreeb( int argc, char *argv[] )
{
    MPI_Comm comm = MPI_COMM_WORLD;
    int dest = 1, src = 0, tag = 1;
    int s1;
    char *buf, *bbuf;
    int smsg[5], rmsg[5];
    int errs = 0, rank, size;
    int bufsize, bsize;

    MTest_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    if (src >= size || dest >= size) {
	int r = src;
	if (dest > r) r = dest;
	fprintf( stderr, "This program requires %d processes\n", r-1 );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    if (rank == src) {
	MPI_Request r;

	MPI_Barrier( MPI_COMM_WORLD );

	/** According to the standard, we must use the PACK_SIZE length of each
	   message in the computation of the message buffer size */
	MPI_Pack_size( 5, MPI_INT, comm, &s1 );
	bufsize = MPI_BSEND_OVERHEAD + s1 + 2000;
	buf = (char *)malloc( bufsize );
	MPI_Buffer_attach( buf, bufsize );

	MTestPrintfMsg( 10, "About create and free Isend request\n" );
	smsg[0] = 10;
	MPI_Isend( &smsg[0], 1, MPI_INT, dest, tag, comm, &r );
	MPI_Request_free( &r );
	if (r != MPI_REQUEST_NULL) {
	    errs++;
	    fprintf( stderr, "Request not set to NULL after request free\n" );
	}
	MTestPrintfMsg( 10, "About create and free Ibsend request\n" );
	smsg[1] = 11;
	MPI_Ibsend( &smsg[1], 1, MPI_INT, dest, tag+1, comm, &r );
	MPI_Request_free( &r );
	if (r != MPI_REQUEST_NULL) {
	    errs++;
	    fprintf( stderr, "Request not set to NULL after request free\n" );
	}
	MTestPrintfMsg( 10, "About create and free Issend request\n" );
	smsg[2] = 12;
	MPI_Issend( &smsg[2], 1, MPI_INT, dest, tag+2, comm, &r );
	MPI_Request_free( &r );
	if (r != MPI_REQUEST_NULL) {
	    errs++;
	    fprintf( stderr, "Request not set to NULL after request free\n" );
	}
	MTestPrintfMsg( 10, "About create and free Irsend request\n" );
	smsg[3] = 13;
	MPI_Irsend( &smsg[3], 1, MPI_INT, dest, tag+3, comm, &r );
	MPI_Request_free( &r );
	if (r != MPI_REQUEST_NULL) {
	    errs++;
	    fprintf( stderr, "Request not set to NULL after request free\n" );
	}
	smsg[4] = 14;
	MPI_Isend( &smsg[4], 1, MPI_INT, dest, tag+4, comm, &r );
	MPI_Wait( &r, MPI_STATUS_IGNORE );

	/** We can't guarantee that messages arrive until the detach */
 	MPI_Buffer_detach( &bbuf, &bsize ); 
    }

    if (rank == dest) {
	MPI_Request r[5];
	int i;

	for (i=0; i<5; i++) {
	    MPI_Irecv( &rmsg[i], 1, MPI_INT, src, tag+i, comm, &r[i] );
	}
	if (rank != src) /** Just in case rank == src */
	    MPI_Barrier( MPI_COMM_WORLD );

	for (i=0; i<4; i++) {
	    MPI_Wait( &r[i], MPI_STATUS_IGNORE );
	    if (rmsg[i] != 10+i) {
		errs++;
		fprintf( stderr, "message %d (%d) should be %d\n", i, rmsg[i], 10+i );
	    }
	}
	/** The MPI standard says that there is no way to use MPI_Request_free
	   safely with receive requests.  A strict MPI implementation may
	   choose to consider these erroreous (an IBM MPI implementation
	   does so)  */
#ifdef USE_STRICT_MPI
	MPI_Wait( &r[4], MPI_STATUS_IGNORE );
#else
	MTestPrintfMsg( 10, "About  free Irecv request\n" );
	MPI_Request_free( &r[4] ); 
#endif
    }

    if (rank != dest && rank != src) {
	MPI_Barrier( MPI_COMM_WORLD );
    }


    MTest_Finalize( errs );

    MPI_Finalize();

    return 0;
}

}