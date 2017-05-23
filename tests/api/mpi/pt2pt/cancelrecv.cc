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
#include <string.h>   /** For memset */

namespace cancelrecv {
int cancelrecv( int argc, char *argv[] )
{
    MPI_Request r[3];
    MPI_Status  s[3];
    int *buf0, *buf1, *buf2;
    int rank, size, src, dest, flag, errs = 0;
    int n0, n1, n2;
    MPI_Comm comm;

    MTest_Init( &argc, &argv );

    MPI_Comm_size( MPI_COMM_WORLD, &size );
    if (size < 2) {
	fprintf( stderr, "Must run with at least 2 processes\n" );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    dest = 0;
    src  = 1;
    comm = MPI_COMM_WORLD;

    n0 = n1 = n2 = 65536;
    buf0 = (int *)malloc( n0 * sizeof(int) );
    buf1 = (int *)malloc( n1 * sizeof(int) );
    buf2 = (int *)malloc( n2 * sizeof(int) );
    if (!buf0 || !buf1 || !buf2) {
	fprintf( stderr, "Unable to allocate buffers of size %d\n", 
		 n0 * (int)sizeof(int) );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    memset( buf0, -1, n0 * sizeof(int) );
    memset( buf1, -1, n0 * sizeof(int) );
    memset( buf2, -1, n0 * sizeof(int) );

    if (rank == dest) {
	MPI_Irecv( buf0, n0, MPI_INT, src, 0, comm, &r[0] );
	MPI_Irecv( buf1, n1, MPI_INT, src, 1, comm, &r[1] );
	MPI_Irecv( buf2, n2, MPI_INT, src, 2, comm, &r[2] );
	
	MPI_Barrier( comm );

	MPI_Cancel( &r[1] );
	MPI_Barrier( comm );
	memset( s, -1, sizeof(s) );
	MPI_Waitall( 3, r, s );
        MPI_Test_cancelled( &s[0], &flag );
        if (flag) {
	    errs++;
	    printf( "request 0 was cancelled!\n" );
	}
        MPI_Test_cancelled( &s[1], &flag );
        if (!flag) {
	    errs++;
	    printf( "request 1 was not cancelled!\n" );
	}
        MPI_Test_cancelled( &s[2], &flag );
        if (flag) {
	    errs++;
	    printf( "request 2 was cancelled!\n" );
	}
	MPI_Barrier( comm );
    }
    if (rank == src) {
	int tflag;
 	MPI_Barrier( comm );
 	MPI_Barrier( comm );
	MPI_Send( buf0, n0, MPI_INT, dest, 0, comm );
	MPI_Isend( buf2, n2, MPI_INT, dest, 2, comm, &r[1] );
	MPI_Isend( buf1, n1, MPI_INT, dest, 4, comm, &r[0] );
	MPI_Cancel( &r[0] );
	memset( s, -3, sizeof(s) );
	s[0].MPI_ERROR = -3;
	s[1].MPI_ERROR = -3;
 	MPI_Testall( 2, r, &tflag, s );
	if (tflag) {
	    MPI_Test_cancelled( &s[0], &flag );
	    if (!flag) {
		errs++;
		printf( "send request 0 was not cancelled!\n" );
	    }
	    MPI_Test_cancelled( &s[1], &flag );
	    if (flag) {
		errs++;
		printf( "send request 1 was cancelled!\n" );
	    }
	}
	else {
	    /** If all requests are not complete, then neither r nor s 
	       may be changed */
	    if ( (s[0].MPI_ERROR) != -3) {
		errs++;
		printf( "Send request status 0 modified. s[0].MPI_ERROR = %x\n",
			s[0].MPI_ERROR );
	    }
	    if ( (s[1].MPI_ERROR) != -3) {
		errs++;
		printf( "Send request status 1 modified. s[1].MPI_ERROR = %x\n",
			s[1].MPI_ERROR );
	    }
	}
	MPI_Barrier( comm );
	while (!tflag) {
	    MPI_Testall( 2, r, &tflag, s );
	}
	MPI_Test_cancelled( &s[0], &flag );
	if (!flag) {
	    errs++;
	    printf( "send request 0 was not cancelled!\n" );
	}
	MPI_Test_cancelled( &s[1], &flag );
	if (flag) {
	    errs++;
	    printf( "send request 1 was cancelled!\n" );
	}
    }
    if (rank != src && rank != dest) {
 	MPI_Barrier( comm );
 	MPI_Barrier( comm );
	MPI_Barrier( comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();

    return 0;
}

}