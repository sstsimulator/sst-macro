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
#include <assert.h>

namespace allred5 {
/**
static char MTEST_Descrip[] = "Test MPI_Allreduce with count greater than the number of processes";
*/

/** We make the error count global so that we can easily control the output
   of error information (in particular, limiting it after the first 10 
   errors */
int errs = 0;

int allred5( int argc, char *argv[] )
{
    MPI_Comm comm;
    MPI_Datatype dtype;
    int count, *bufin, *bufout, size, i, minsize=1;

    MTest_Init( &argc, &argv );
    
    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) {
	    continue;
	}
	MPI_Comm_size( comm, &size );
	count = size * 2;
	bufin = (int *)malloc( count * sizeof(int) );
	bufout = (int *)malloc( count * sizeof(int) );
	if (!bufin || !bufout) {
	    fprintf( stderr, "Unable to allocated space for buffers (%d)\n",
		     count );
	    MPI_Abort( MPI_COMM_WORLD, 1 );
	}
	for (i=0; i<count; i++) {
	    bufin[i] = i;
	    bufout[i] = -1;
	}

	dtype = MPI_INT;
	MPI_Allreduce( bufin, bufout, count, dtype, MPI_SUM, comm );
	/** Check output */
	for (i=0; i<count; i++) {
	    if (bufout[i] != i * size) {
		fprintf( stderr, "Expected bufout[%d] = %d but found %d\n",
			 i, i * size, bufout[i] );
		errs++;
	    }
	}
	free( bufin );
	free( bufout );
	MTestFreeComm( &comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}