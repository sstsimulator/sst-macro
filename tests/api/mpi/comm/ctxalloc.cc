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

namespace ctxalloc {
/**
 * This program tests the allocation (and deallocation) of contexts.
 * 
 */
int ctxalloc( int argc, char **argv )
{
    int errs = 0;
    int i, j, err;
    MPI_Comm newcomm1, newcomm2[200];

    MTest_Init( &argc, &argv );

    /** Get a separate communicator to duplicate */
    MPI_Comm_dup( MPI_COMM_WORLD, &newcomm1 );

    MPI_Errhandler_set( newcomm1, MPI_ERRORS_RETURN );
    /** Allocate many communicators in batches, then free them */
    for (i=0; i<1000; i++) {
	for (j=0; j<200; j++) {
	    err = MPI_Comm_dup( newcomm1, &newcomm2[j] );
	    if (err) {
		errs++;
		if (errs < 10) {
		    fprintf( stderr, "Failed to duplicate communicator for (%d,%d)\n", i, j );
		    MTestPrintError( err );
		}
	    }
	}
	for (j=0; j<200; j++) {
	    err = MPI_Comm_free( &newcomm2[j] );
	    if (err) {
		errs++;
		if (errs < 10) {
		    fprintf( stderr, "Failed to free %d,%d\n", i, j );
		    MTestPrintError( err );
		}
	    }
	}
    }
    err = MPI_Comm_free( &newcomm1 );
    if (err) {
	errs++;
	fprintf( stderr, "Failed to free newcomm1\n" );
	MTestPrintError( err );
    }
      
    MTest_Finalize( errs );

    MPI_Finalize();

    return 0;
}

}