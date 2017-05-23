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

namespace scatter3 {
/** This example sends contiguous data and receives a vector on some nodes
   and contiguous data on others.  There is some evidence that some
   MPI implementations do not check recvcount on the root process; this
   test checks for that case 
*/

int scatter3( int argc, char **argv )
{
    MPI_Datatype vec;
    double *vecin, *vecout, ivalue;
    int    root, i, n, stride, errs = 0;
    int    rank, size;
    MPI_Aint vextent;

    MTest_Init( &argc, &argv );
    
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    n = 12;
    stride = 10;
    /** Note that vecout really needs to be only (n-1)*stride+1 doubles, but
       this is easier and allows a little extra room if there is a bug */
    vecout = (double *)malloc( n * stride * sizeof(double) );
    vecin  = (double *)malloc( n * size * sizeof(double) );
    
    MPI_Type_vector( n, 1, stride, MPI_DOUBLE, &vec );
    MPI_Type_commit( &vec );
    MPI_Type_extent( vec, &vextent );
    if (vextent != ((n-1)*(MPI_Aint)stride + 1) * sizeof(double) ) {
	errs++;
	printf( "Vector extent is %ld, should be %ld\n", 
		 (long) vextent, (long)(((n-1)*stride+1)*sizeof(double)) );
    }
    /** Note that the exted of type vector is from the first to the
       last element, not n*stride.
       E.g., with n=1, the extent is a single double */

    for (i=0; i<n*size; i++) vecin[i] = (double)i;
    for (root=0; root<size; root++) {
	for (i=0; i<n*stride; i++) vecout[i] = -1.0;
	if (rank == root) {
	    /** Receive into a vector */
	    MPI_Scatter( vecin, n, MPI_DOUBLE, vecout, 1, vec, 
			 root, MPI_COMM_WORLD );
	    for (i=0; i<n; i++) {
		ivalue = n*root + i;
		if (vecout[i*stride] != ivalue) {
		    errs++;
		    printf( "[%d] Expected %f but found %f for vecout[%d] on root\n", 
			    rank, ivalue, vecout[i*stride], i *stride );
		}
	    }
	}
	else {
	    /** Receive into contiguous data */
	    MPI_Scatter( NULL, -1, MPI_DATATYPE_NULL, vecout, n, MPI_DOUBLE,
			 root, MPI_COMM_WORLD );
	    for (i=0; i<n; i++) {
		ivalue = rank * n + i;
		if (vecout[i] != ivalue) {
		    printf( "[%d] Expected %f but found %f for vecout[%d]\n", 
			    rank, ivalue, vecout[i], i );
		    errs++;
		}
	    }
	}
    }
    
    MTest_Finalize( errs );
    MPI_Type_free( &vec );
    MPI_Finalize();
    return 0;
}


}