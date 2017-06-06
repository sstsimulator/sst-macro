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

namespace tfree {
/**
static char MTEST_Descrip[] = "Test that freed datatypes have reference count semantics";
*/

#define VEC_NELM 128
#define VEC_STRIDE 8

int tfree( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest, i;
    MPI_Comm      comm;
    MPI_Status    status;
    MPI_Request   req;
    MPI_Datatype  strideType;
    MPI_Datatype  tmpType[1024];
    int           *buf = 0;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    if (size < 2) {
	fprintf( stderr, "This test requires at least two processes." );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    source  = 0;
    dest    = size - 1;

    /** 
       The idea here is to create a simple but non-contig datatype,
       perform an irecv with it, free it, and then create 
       many new datatypes.  While not a complete test, if the datatype
       was freed and the space was reused, this test may detect 
       that error 
       A similar test for sends might work by sending a large enough message
       to force the use of rendezvous send. 
    */
    MPI_Type_vector( VEC_NELM, 1, VEC_STRIDE, MPI_INT, &strideType );
    MPI_Type_commit( &strideType );

    if (rank == dest) {
	buf = (int *)malloc( VEC_NELM * VEC_STRIDE * sizeof(int) );
	for (i=0; i<VEC_NELM*VEC_STRIDE; i++) buf[i] = -i;
	MPI_Irecv( buf, 1, strideType, source, 0, comm, &req );
	MPI_Type_free( &strideType );

	for (i=0; i<1024; i++) {
	    MPI_Type_vector( VEC_NELM, 1, 1, MPI_INT, &tmpType[i] );
	    MPI_Type_commit( &tmpType[i] );
	}

	MPI_Sendrecv( 0, 0, MPI_INT, source, 1, 
		      0, 0, MPI_INT, source, 1, comm, &status );

	MPI_Wait( &req, &status );
	for (i=0; i<VEC_NELM; i++) {
	    if (buf[VEC_STRIDE*i] != i) {
		errs++;
		if (errs < 10) {
		    printf( "buf[%d] = %d, expected %d\n", VEC_STRIDE*i, 
			    buf[VEC_STRIDE*i], i );
		}
	    }
	}
	for (i=0; i<1024; i++) {
	    MPI_Type_free( &tmpType[i] );
	}
	free( buf );
    }
    else if (rank == source) {
	buf = (int *)malloc( VEC_NELM * sizeof(int) );
	for (i=0; i<VEC_NELM; i++) buf[i] = i;
	/** Synchronize with the receiver */
	MPI_Sendrecv( 0, 0, MPI_INT, dest, 1, 
		      0, 0, MPI_INT, dest, 1, comm, &status );
	MPI_Send( buf, VEC_NELM, MPI_INT, dest, 0, comm );
	free( buf );
    }

    /** Clean up the strideType */
    if (rank != dest) {
	MPI_Type_free( &strideType );
    }


    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}