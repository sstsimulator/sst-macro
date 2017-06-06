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

namespace rcancel {
/**
static char MTEST_Descrip[] = "Test of various receive cancel calls, with multiple requests to cancel";
*/

int rcancel( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest;
    MPI_Comm      comm;
    MPI_Status    status;
    MPI_Request   req[4];
    static int bufsizes[4] = { 1, 100, 10000, 1000000 };
    char *bufs[4];
    int  flag, i;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    source = 0;
    dest   = size - 1;

    if (rank == source) {
	MPI_Send( MPI_BOTTOM, 0, MPI_CHAR, dest, 1, MPI_COMM_WORLD );
    }
    else if (rank == dest) {
	/** Create 3 requests to cancel, plus one to use.  
	   Then receive one message and exit */ 
	for (i=0; i<4; i++) {
	    bufs[i] = (char *) malloc( bufsizes[i] );
	    MPI_Irecv( bufs[i], bufsizes[i], MPI_CHAR, source, 
		       i, MPI_COMM_WORLD, &req[i] );
	}
	/** Now, cancel them in a more interesting order, to ensure that the
	   queue operation work properly */
	MPI_Cancel( &req[2] );
	MPI_Wait( &req[2], &status );
	MTestPrintfMsg( 1, "Completed wait on irecv[2]\n" );
	MPI_Test_cancelled( &status, &flag );
	if (!flag) {
	    errs ++;
	    printf( "Failed to cancel a Irecv[2] request\n" );
	    fflush(stdout);
	}
	MPI_Cancel( &req[3] );
	MPI_Wait( &req[3], &status );
	MTestPrintfMsg( 1, "Completed wait on irecv[3]\n" );
	MPI_Test_cancelled( &status, &flag );
	if (!flag) {
	    errs ++;
	    printf( "Failed to cancel a Irecv[3] request\n" );
	    fflush(stdout);
	}
	MPI_Cancel( &req[0] );
	MPI_Wait( &req[0], &status );
	MTestPrintfMsg( 1, "Completed wait on irecv[0]\n" );
	MPI_Test_cancelled( &status, &flag );
	if (!flag) {
	    errs ++;
	    printf( "Failed to cancel a Irecv[0] request\n" );
	    fflush(stdout);
	}
	MPI_Wait( &req[1], &status );
	MPI_Test_cancelled( &status, &flag );
	if (flag) {
	    errs ++;
	    printf( "Incorrectly cancelled Irecv[1]\n" ); fflush(stdout);
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}