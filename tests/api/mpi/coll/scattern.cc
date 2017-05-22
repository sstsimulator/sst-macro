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
#include <stdlib.h>
#include <stdio.h>

namespace scattern {

/** This example sends a vector and receives individual elements */

int scattern( int argc, char **argv )
{
    MPI_Datatype vec;
    double *vecin, *vecout, ivalue;
    int    root, i, n, stride, err = 0;
    int    rank, size;

    MPI_Init( &argc, &argv );
    
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    n = 12;
    stride = 10;
    vecin = (double *)malloc( n * stride * size * sizeof(double) );
    vecout = (double *)malloc( n * sizeof(double) );
    
    MPI_Type_vector( n, 1, stride, MPI_DOUBLE, &vec );
    MPI_Type_commit( &vec );

    for (i=0; i<n*stride*size; i++) vecin[i] = (double)i;
    for (root=0; root<size; root++) {
	for (i=0; i<n; i++) vecout[i] = -1.0;
	MPI_Scatter( vecin, 1, vec, vecout, n, MPI_DOUBLE, root, 
		     MPI_COMM_WORLD );
	ivalue = rank * ((n-1) * stride + 1);
	for (i=0; i<n; i++) {
	    if (vecout[i] != ivalue) {
		printf( "Expected %f but found %f\n", 
			ivalue, vecout[i] );
		err++;
	    }
	    ivalue += stride;
	}
    }
    i = err;
    MPI_Allreduce( &i, &err, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD );
    if (rank == 0) {
	if (err > 0) printf( "Found %d errors!\n", err );
	else         printf( " No Errors\n" );
    }
    MPI_Type_free( &vec );
    MPI_Finalize();
    return 0;
       
}


}