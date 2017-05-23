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

namespace cmfree {
/**
static char MTEST_Descrip[] = "Test that communicators have reference count semantics";
*/

#define NELM 128
#define NCOMM 10

int cmfree( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size, source, dest, i;
    MPI_Comm      comm;
    MPI_Comm      tmpComm[NCOMM];
    MPI_Status    status;
    MPI_Request   req;
    int           *buf=0;

    MTest_Init( &argc, &argv );

    MPI_Comm_dup( MPI_COMM_WORLD, &comm );

    /** This is similar to the datatype test, except that we post
       an irecv on a simple data buffer but use a rank-reordered communicator.
       In this case, an error in handling the reference count will most 
       likely cause the program to hang, so this should be run only
       if (a) you are confident that the code is correct or (b) 
       a timeout is set for mpiexec 
    */

    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    if (size < 2) {
	fprintf( stderr, "This test requires at least two processes." );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    source  = 0;
    dest    = size - 1;

    if (rank == dest) {
	buf = (int *)malloc( NELM * sizeof(int) );
	for (i=0; i<NELM; i++) buf[i] = -i;
	MPI_Irecv( buf, NELM, MPI_INT, source, 0, comm, &req );
	MPI_Comm_free( &comm );

	if (comm != MPI_COMM_NULL) {
	    errs++;
	    printf( "Freed comm was not set to COMM_NULL\n" );
	}

	for (i=0; i<NCOMM; i++) {
	    MPI_Comm_split( MPI_COMM_WORLD, 0, size - rank, &tmpComm[i] );
	}

	MPI_Sendrecv( 0, 0, MPI_INT, source, 1, 
		      0, 0, MPI_INT, source, 1, MPI_COMM_WORLD, &status );

	MPI_Wait( &req, &status );
	for (i=0; i<NELM; i++) {
	    if (buf[i] != i) {
		errs++;
		if (errs < 10) {
		    printf( "buf[%d] = %d, expected %d\n", i, buf[i], i );
		}
	    }
	}
	for (i=0; i<NCOMM; i++) {
	    MPI_Comm_free( &tmpComm[i] );
	}
	free( buf );
    }
    else if (rank == source) {
	buf = (int *)malloc( NELM * sizeof(int) );
	for (i=0; i<NELM; i++) buf[i] = i;

	for (i=0; i<NCOMM; i++) {
	    MPI_Comm_split( MPI_COMM_WORLD, 0, size - rank, &tmpComm[i] );
	}
	/** Synchronize with the receiver */
	MPI_Sendrecv( 0, 0, MPI_INT, dest, 1, 
		      0, 0, MPI_INT, dest, 1, MPI_COMM_WORLD, &status );
	MPI_Send( buf, NELM, MPI_INT, dest, 0, comm );
	free( buf );
    }
    else {
	for (i=0; i<NCOMM; i++) {
	    MPI_Comm_split( MPI_COMM_WORLD, 0, size - rank, &tmpComm[i] );
	}
    }

    MPI_Barrier( MPI_COMM_WORLD );

    if (rank != dest) {
	/** Clean up the communicators */
	for (i=0; i<NCOMM; i++) {
	    MPI_Comm_free( &tmpComm[i] );
	}
    }
    if (comm != MPI_COMM_NULL) {
	MPI_Comm_free( &comm );
    }
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}