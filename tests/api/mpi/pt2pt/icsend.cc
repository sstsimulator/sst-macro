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

namespace icsend {
/**
static char MTEST_Descrip[] = "Simple test of intercommunicator send and receive";
*/

int icsend( int argc, char *argv[] )
{
    int errs = 0;
    int leftGroup, buf, rank, remote_size, i;
    MPI_Comm comm;
    MPI_Status status;

    MTest_Init( &argc, &argv );

    while (MTestGetIntercomm( &comm, &leftGroup, 4 )) {
        if (comm == MPI_COMM_NULL) continue;

	if (leftGroup) {
	    MPI_Comm_rank( comm, &rank );
	    buf = rank;
	    MPI_Send( &buf, 1, MPI_INT, 0, 0, comm );
	}
	else {
	    MPI_Comm_remote_size( comm, &remote_size );
	    MPI_Comm_rank( comm, &rank );
	    if (rank == 0) {
		for (i=0; i<remote_size; i++) {
		    buf = -1;
		    MPI_Recv( &buf, 1, MPI_INT, i, 0, comm, &status );
		    if (buf != i) {
			errs++;
			fprintf( stderr, "buf = %d, should be %d\n", buf, i );
		    }
		}
	    }
	}
	/** Now, reverse it and send back */
	if (!leftGroup) {
	    MPI_Comm_rank( comm, &rank );
	    buf = rank;
	    MPI_Send( &buf, 1, MPI_INT, 0, 0, comm );
	}
	else {
	    MPI_Comm_remote_size( comm, &remote_size );
	    MPI_Comm_rank( comm, &rank );
	    if (rank == 0) {
		for (i=0; i<remote_size; i++) {
		    buf = -1;
		    MPI_Recv( &buf, 1, MPI_INT, i, 0, comm, &status );
		    if (buf != i) {
			errs++;
			fprintf( stderr, "buf = %d, should be %d\n", buf, i );
		    }
		}
	    }
	}
        MTestFreeComm(&comm);
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}