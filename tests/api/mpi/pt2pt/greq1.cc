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

namespace greq1 {
/**
static char MTEST_Descrip[] = "Simple test of generalized requests";
*/


int query_fn( void *extra_state, MPI_Status *status );
int query_fn( void *extra_state, MPI_Status *status )
{
    /** Set a default status */
    status->MPI_SOURCE = MPI_UNDEFINED;
    status->MPI_TAG    = MPI_UNDEFINED;
    MPI_Status_set_cancelled( status, 0 );
    MPI_Status_set_elements( status, MPI_BYTE, 0 );
    return 0;
}
int free_fn( void *extra_state );
int free_fn( void *extra_state )
{
    int *b = (int *)extra_state;
    if (b) *b = *b - 1;
    /** The value returned by the free function is the error code
       returned by the wait/test function */
    return 0;
}
int cancel_fn( void *extra_state, int complete );
int cancel_fn( void *extra_state, int complete )
{
    return 0;
}

/**
 * This is a very simple test of generalized requests.  Normally, the
 * MPI_Grequest_complete function would be called from another routine,
 * often running in a separate thread.  This simple code allows us to
 * check that requests can be created, tested, and waited on in the
 * case where the request is complete before the wait is called.  
 *
 * Note that MPI did *not* define a routine that can be called within
 * test or wait to advance the state of a generalized request.  
 * Most uses of generalized requests will need to use a separate thread.
 */
int greq1( int argc, char *argv[] )
{
    int errs = 0;
    int counter, flag;
    MPI_Status    status;
    MPI_Request   request;

    MTest_Init( &argc, &argv );

    MPI_Grequest_start( query_fn, free_fn, cancel_fn, NULL, &request );
    
    MPI_Test( &request, &flag, &status );
    if (flag) {
	errs++;
	fprintf( stderr, "Generalized request marked as complete\n" );
    }

    MPI_Grequest_complete( request );

    MPI_Wait( &request, &status );

    counter = 1;
    MPI_Grequest_start( query_fn, free_fn, cancel_fn, &counter, &request );
    MPI_Grequest_complete( request );
    MPI_Wait( &request, MPI_STATUS_IGNORE );
    
    if (counter) {
	errs++;
	fprintf( stderr, "Free routine not called, or not called with extra_data" );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}