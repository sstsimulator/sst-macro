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

namespace tresized {
/**
static char MTEST_Descrip[] = "Test of type resized";
*/

int tresized( int argc, char *argv[] )
{
    int errs = 0, i;
    int rank, size, source, dest;
    int count; 
    int *buf; 
    MPI_Comm      comm;
    MPI_Status    status;
    MPI_Datatype  newtype;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    /** Determine the sender and receiver */
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );
    source = 0;
    dest   = size - 1;
	
    MPI_Type_create_resized( MPI_INT, 0, 3 * sizeof(int), &newtype );
    MPI_Type_commit( &newtype );
    for (count = 1; count < 65000; count = count * 2) {
	buf = (int *)malloc( count * 3 * sizeof(int) );
	if (!buf) {
	    MPI_Abort( comm, 1 );
	}
	for (i=0; i<3*count; i++) buf[i] = -1;
	if (rank == source) {
	    for (i=0; i<count; i++) buf[3*i] = i;
	    MPI_Send( buf, count, newtype, dest, 0, comm );
	    MPI_Send( buf, count, newtype, dest, 1, comm );
	}
	else if (rank == dest) {
	    MPI_Recv( buf, count, MPI_INT, source, 0, comm, &status );
	    for (i=0; i<count; i++) {
		if (buf[i] != i) {
		    errs++;
		    if (errs < 10) {
			printf( "buf[%d] = %d\n", i, buf[i] );
		    }
		}
	    }
	    for (i=0; i<count*3; i++) buf[i] = -1;
	    MPI_Recv( buf, count, newtype, source, 1, comm, &status );
	    for (i=0; i<count; i++) {
		if (buf[3*i] != i) {
		    errs++;
		    if (errs < 10) {
			printf( "buf[3*%d] = %d\n", i, buf[i] );
		    }
		}
	    }
	}
    }

    MPI_Type_free( &newtype );

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}