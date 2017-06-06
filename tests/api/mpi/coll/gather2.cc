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
#include "mpitest.h"
#include <stdlib.h>
#include <stdio.h>

namespace gather2{
/** Gather data from a vector to contiguous.  Use IN_PLACE */

int gather2( int argc, char **argv )
{
    MPI_Datatype vec;
    double *vecin, *vecout;
    MPI_Comm comm;
    int    count, minsize = 2;
    int    root, i, n, stride, errs = 0;
    int    rank, size;

    MTest_Init( &argc, &argv );

    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;
	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );
	
	for (root=0; root<size; root++) {

      for (int iter = 0; iter < 1; ++iter) {
		n = 12;
    int blocklength = 1;
    stride = 10;
		vecin = (double *)malloc( n * stride * size * sizeof(double) );
		vecout = (double *)malloc( size * n * sizeof(double) );
		
    MPI_Type_vector( n, blocklength, stride, MPI_DOUBLE, &vec );
		MPI_Type_commit( &vec );
		
		for (i=0; i<n*stride; i++) vecin[i] =-2;
		for (i=0; i<n; i++) vecin[i*stride] = rank * n + i;
		
		if (rank == root) {
		    for (i=0; i<n; i++) {
			vecout[rank*n+i] = rank*n+i;
		    }
		    MPI_Gather( MPI_IN_PLACE, -1, MPI_DATATYPE_NULL, 
				vecout, n, MPI_DOUBLE, root, comm );
		}
		else {
        MPI_Gather( vecin, blocklength, vec, NULL, -1, MPI_DATATYPE_NULL,
				root, comm );
		}
		if (rank == root) {
		    for (i=0; i<n*size; i++) {
			if (vecout[i] != i) {
			    errs++;
			    if (errs < 10) {
				fprintf( stderr, "vecout[%d]=%d\n",
					 i, (int)vecout[i] );
			    }
			}
		    }
		}
		MPI_Type_free( &vec );
		free( vecin );
		free( vecout );
	    }
	}
	MTestFreeComm( &comm );
    }

    /** do a zero length gather */
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    if ( rank == 0 ) {
	MPI_Gather( MPI_IN_PLACE, -1, MPI_DATATYPE_NULL, NULL, 0, MPI_BYTE, 0,
		    MPI_COMM_WORLD );
    } else {
	MPI_Gather( NULL, 0, MPI_BYTE, NULL, 0, MPI_BYTE, 0, MPI_COMM_WORLD );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}


}