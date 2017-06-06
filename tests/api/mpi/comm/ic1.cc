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

namespace ic1 {

int ic1( int argc, char *argv[] )
{
    MPI_Comm intercomm;
    int      remote_rank, rank, size, errs = 0;
    volatile int trigger;

    MTest_Init( &argc, &argv );

    trigger = 1;
/**    while (trigger) ; */

    MPI_Comm_size( MPI_COMM_WORLD, &size );
    if (size < 2) {
	printf( "Size must be at least 2\n" );
	MPI_Abort( MPI_COMM_WORLD, 0 );
    }

    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    /** Make an intercomm of the first two elements of comm_world */
    if (rank < 2) {
	int lrank = rank, rrank = -1;
	MPI_Status status;

	remote_rank = 1 - rank;
	MPI_Intercomm_create( MPI_COMM_SELF, 0,
			      MPI_COMM_WORLD, remote_rank, 27, 
			      &intercomm );

	/** Now, communicate between them */
	MPI_Sendrecv( &lrank, 1, MPI_INT, 0, 13, 
		      &rrank, 1, MPI_INT, 0, 13, intercomm, &status );

	if (rrank != remote_rank) {
	    errs++;
	    printf( "%d Expected %d but received %d\n", 
		    rank, remote_rank, rrank );
	}

	MPI_Comm_free( &intercomm );
    }
    
    /** The next test should create an intercomm with groups of different
       sizes FIXME */

    MTest_Finalize( errs );
    MPI_Finalize();
    
    return 0;
}

}