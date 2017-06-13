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

namespace duptest {
int duptest( int argc, char **argv )
{
    int errs = 0;
    int rank, size, wrank, wsize, dest, a, b;
    MPI_Comm newcomm;
    MPI_Status status;

    MTest_Init( &argc, &argv );

    /** Can we run comm dup at all? */
    MPI_Comm_dup( MPI_COMM_WORLD, &newcomm );

    /** Check basic properties */
    MPI_Comm_size( MPI_COMM_WORLD, &wsize );
    MPI_Comm_rank( MPI_COMM_WORLD, &wrank );
    MPI_Comm_size( newcomm, &size );
    MPI_Comm_rank( newcomm, &rank );
    
    if (size != wsize || rank != wrank) {
	errs++;
	fprintf( stderr, "Size (%d) or rank (%d) wrong\n", size, rank );
	fflush( stderr );
    }

    /** Can we communicate with this new communicator? */
    dest = MPI_PROC_NULL;
    if (rank == 0) {
	dest = size - 1;
	a = rank;
	b = -1;
	MPI_Sendrecv( &a, 1, MPI_INT, dest, 0,
		      &b, 1, MPI_INT, dest, 0, newcomm, &status );
	if (b != dest) {
	    errs++;
	    fprintf( stderr, "Received %d expected %d on %d\n", b, dest, rank );
	    fflush( stderr );
	}
	if (status.MPI_SOURCE != dest) {
	    errs++;
	    fprintf( stderr, "Source not set correctly in status on %d\n", 
		     rank );
	    fflush( stderr );
	}
    }
    else if (rank == size-1) { 
	dest = 0;
	a = rank;
	b = -1;
	MPI_Sendrecv( &a, 1, MPI_INT, dest, 0,
		      &b, 1, MPI_INT, dest, 0, newcomm, &status );
	if (b != dest) {
	    errs++;
	    fprintf( stderr, "Received %d expected %d on %d\n", b, dest, rank );
	    fflush( stderr );
	}
	if (status.MPI_SOURCE != dest) {
	    errs++;
	    fprintf( stderr, "Source not set correctly in status on %d\n", 
		     rank );
	    fflush( stderr );
	}
    }

    MPI_Comm_free( &newcomm );

    MTest_Finalize( errs );

    MPI_Finalize();

    return 0;
}

}