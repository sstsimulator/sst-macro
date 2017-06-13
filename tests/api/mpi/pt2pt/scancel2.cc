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

namespace scancel2 {
/**
static char MTEST_Descrip[] = "Test of send cancel (failure) calls";
*/

int scancel2( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest;
    MPI_Comm      comm;
    MPI_Status    status;
    MPI_Request   req;
    static int bufsizes[4] = { 1, 100, 10000, 1000000 };
    char *buf;
    int  cs, flag, n;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    source = 0;
    dest   = size - 1;

    MTestPrintfMsg( 1, "Starting scancel test\n" );

    for (cs=0; cs<4; cs++) {
	n = bufsizes[cs];
	buf = (char *)malloc( n );
	if (!buf) {
	    fprintf( stderr, "Unable to allocate %d bytes\n", n );
	    MPI_Abort( MPI_COMM_WORLD, 1 );
	}

	if (rank == source) {
	    MTestPrintfMsg( 1, "(%d) About to create isend and cancel\n",cs );
	    MPI_Isend( buf, n, MPI_CHAR, dest, cs+n+1, comm, &req );
	    MPI_Barrier( comm );
	    MPI_Cancel( &req );
	    MPI_Wait( &req, &status );
	    MTestPrintfMsg( 1, "Completed wait on isend\n" );
	    MPI_Test_cancelled( &status, &flag );
	    if (flag) {
		errs ++;
		printf( "Cancelled a matched Isend request (msg size = %d)!\n",
			n );
		fflush(stdout);
	    }
	    else
	    {
		n = 0;
	    }
	    /** Send the size, zero for not cancelled (success) */
	    MPI_Send( &n, 1, MPI_INT, dest, 123, comm );
	}
	else if (rank == dest)
	{
	    MPI_Recv( buf, n, MPI_CHAR, source, cs+n+1, comm, &status );
	    MPI_Barrier( comm );
	    MPI_Recv( &n, 1, MPI_INT, source, 123, comm, &status );
	}
	else {
	    MPI_Barrier( comm );
	}

	MPI_Barrier( comm );
	free( buf );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}