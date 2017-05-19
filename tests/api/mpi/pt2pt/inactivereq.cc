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

namespace inactivereq {
/** This test program checks that the point-to-point completion routines
   can be applied to an inactive persistent request, as required by the 
   MPI-1 standard. See section 3.7.3, for example, 

   One is allowed to call MPI TEST with a null or inactive request argument. 
   In such a case the operation returns with flag = true and empty status.

*/

int StatusEmpty( MPI_Status *s );
int StatusEmpty( MPI_Status *s )
{
    int errs = 0;
    int count = 10;

    if (s->MPI_TAG != MPI_ANY_TAG) {
	errs++;
	printf( "MPI_TAG not MPI_ANY_TAG in status\n" );
    }
    if (s->MPI_SOURCE != MPI_ANY_SOURCE) {
	errs++;
	printf( "MPI_SOURCE not MPI_ANY_SOURCE in status\n" );
    }
    MPI_Get_count( s, MPI_INT, &count );
    if (count != 0) {
	errs++;
	printf( "count in status is not 0\n" );
    }
    /** Return true only if status passed all tests */
    return errs ? 0 : 1;
}

int inactivereq(int argc, char *argv[])
{
    MPI_Request r;
    MPI_Status  s;
    int errs = 0;
    int flag;
    int buf[10];
    int rbuf[10];
    int tag = 27;
    int dest = 0;
    int rank, size;

    MTest_Init( &argc, &argv );

    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    /** Create a persistent send request */
    MPI_Send_init( buf, 10, MPI_INT, dest, tag, MPI_COMM_WORLD, &r );

    flag = 0;
    s.MPI_TAG = 10;
    s.MPI_SOURCE = 10;
    MPI_Test( &r, &flag, &s );
    if (!flag) {
	errs++;
	printf( "Flag not true after MPI_Test (send)\n" );
	printf( "Aborting further tests to avoid hanging in MPI_Wait\n" );
	MTest_Finalize( errs );
	MPI_Finalize();
	return 0;
    }
    if (!StatusEmpty( &s )) {
	errs++;
	printf( "Status not empty after MPI_Test (send)\n" );
    }

    s.MPI_TAG = 10;
    s.MPI_SOURCE = 10;
    MPI_Wait( &r, &s );
    if (!StatusEmpty( &s )) {
	errs++;
	printf( "Status not empty after MPI_Wait (send)\n" );
    }

    /** Now try to use that request, then check again */
    if (rank == 0) {
	int i;
	MPI_Request *rr = (MPI_Request *)malloc(size * sizeof(MPI_Request));
	for (i=0; i<size; i++) {
	    MPI_Irecv( rbuf, 10, MPI_INT, i, tag, MPI_COMM_WORLD, &rr[i] );
	}
	MPI_Start( &r );
	MPI_Wait( &r, &s );
	MPI_Waitall( size, rr, MPI_STATUSES_IGNORE );
    }
    else {
	MPI_Start( &r );
	MPI_Wait( &r, &s );
    }

    flag = 0;
    s.MPI_TAG = 10;
    s.MPI_SOURCE = 10;
    MPI_Test( &r, &flag, &s );
    if (!flag) {
	errs++;
	printf( "Flag not true after MPI_Test (send)\n" );
	printf( "Aborting further tests to avoid hanging in MPI_Wait\n" );
	MTest_Finalize( errs );
	MPI_Finalize();
	return 0;
    }
    if (!StatusEmpty( &s )) {
	errs++;
	printf( "Status not empty after MPI_Test (send)\n" );
    }

    s.MPI_TAG = 10;
    s.MPI_SOURCE = 10;
    MPI_Wait( &r, &s );
    if (!StatusEmpty( &s )) {
	errs++;
	printf( "Status not empty after MPI_Wait (send)\n" );
    }

    

    MPI_Request_free( &r );

    /** Create a persistent receive request */
    MPI_Recv_init( buf, 10, MPI_INT, dest, tag, MPI_COMM_WORLD, &r );

    flag = 0;
    s.MPI_TAG = 10;
    s.MPI_SOURCE = 10;
    MPI_Test( &r, &flag, &s );
    if (!flag) {
	errs++;
	printf( "Flag not true after MPI_Test (recv)\n" );
	printf( "Aborting further tests to avoid hanging in MPI_Wait\n" );
	MTest_Finalize( errs );
	MPI_Finalize();
	return 0;
    }
    if (!StatusEmpty( &s )) {
	errs++;
	printf( "Status not empty after MPI_Test (recv)\n" );
    }

    s.MPI_TAG = 10;
    s.MPI_SOURCE = 10;
    MPI_Wait( &r, &s );
    if (!StatusEmpty( &s )) {
	errs++;
	printf( "Status not empty after MPI_Wait (recv)\n" );
    }

    MPI_Request_free( &r );
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}