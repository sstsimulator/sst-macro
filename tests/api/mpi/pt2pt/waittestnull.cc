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

namespace waittestnull {
/** 
 * This program checks that the various MPI_Test and MPI_Wait routines 
 * allow both null requests and in the multiple completion cases, empty
 * lists of requests.
 */

int waittestnull(int argc, char **argv)
{
    int errs = 0;
    MPI_Status status, *status_array = 0;
    int count = 0, flag, idx, rc, errlen, *indices=0, outcnt;
    MPI_Request *reqs = 0;
    char errmsg[MPI_MAX_ERROR_STRING];

    MTest_Init(&argc, &argv);

    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    rc = MPI_Testall( count, reqs, &flag, status_array );
    if (rc != MPI_SUCCESS) {
	MPI_Error_string( rc, errmsg, &errlen );
	printf( "MPI_Testall returned failure: %s\n", errmsg );
	errs ++;
    }
    else if (!flag) {
	printf( "MPI_Testall( 0, ... ) did not return a true flag\n") ;
	errs++;
    }

    rc = MPI_Waitall( count, reqs, status_array );
    if (rc != MPI_SUCCESS) {
	MPI_Error_string( rc, errmsg, &errlen );
	printf( "MPI_Waitall returned failure: %s\n", errmsg );
	errs ++;
    }

    rc = MPI_Testany( count, reqs, &idx, &flag, &status );
    if (rc != MPI_SUCCESS) {
	MPI_Error_string( rc, errmsg, &errlen );
	printf( "MPI_Testany returned failure: %s\n", errmsg );
	errs ++;
    }
    else if (!flag) {
	printf( "MPI_Testany( 0, ... ) did not return a true flag\n") ;
	errs++;
    }

    rc = MPI_Waitany( count, reqs, &idx, &status );
    if (rc != MPI_SUCCESS) {
	MPI_Error_string( rc, errmsg, &errlen );
	printf( "MPI_Waitany returned failure: %s\n", errmsg );
	errs ++;
    }

    rc = MPI_Testsome( count, reqs, &outcnt, indices, status_array );
    if (rc != MPI_SUCCESS) {
	MPI_Error_string( rc, errmsg, &errlen );
	printf( "MPI_Testsome returned failure: %s\n", errmsg );
	errs ++;
    }

    rc = MPI_Waitsome( count, reqs, &outcnt, indices, status_array );
    if (rc != MPI_SUCCESS) {
	MPI_Error_string( rc, errmsg, &errlen );
	printf( "MPI_Waitsome returned failure: %s\n", errmsg );
	errs ++;
    }
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}