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
#include <sstmac/replacements/mpi.h>
#include "mpitest.h" 

namespace bsend4 {

#undef BUFSIZE
#define BUFSIZE 2000
int bsend4( int argc, char *argv[] )
{
    MPI_Status status;
    MPI_Request request;
    int a[10], b[10];
    int buf[BUFSIZE], *bptr, bl, i, j, rank, size, errs=0;

    MTest_Init( 0, 0 );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Buffer_attach( buf, BUFSIZE );

    for (j=0; j<10; j++) {
	for (i=0; i<10; i++) {
	    a[i] = (rank + 10 * j) * size + i;
	}
	MPI_Ibsend( a, 10, MPI_INT, 0, 27+j, MPI_COMM_WORLD, &request );
	MPI_Wait( &request, &status );
    }
    if (rank == 0) {

	for (i=0; i<size; i++) {
	    for (j=0; j<10; j++) {
		int k;
		status.MPI_TAG = -10;
		status.MPI_SOURCE = -20;
		MPI_Recv( b, 10, MPI_INT, i, 27+j, MPI_COMM_WORLD, &status );
    
		if (status.MPI_TAG != 27+j) {
		    errs++;
		    printf( "Wrong tag = %d\n", status.MPI_TAG );
		}
		if (status.MPI_SOURCE != i) {
		    errs++;
		    printf( "Wrong source = %d\n", status.MPI_SOURCE );
		}
		for (k=0; k<10; k++) {
		    if (b[k] != (i + 10 * j) * size + k) {
			errs ++;
			printf( "received b[%d] = %d from %d tag %d\n",
				k, b[k], i, 27+j );
		    }
		}
	    }
	}
    }
    MPI_Buffer_detach( &bptr, &bl );

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}