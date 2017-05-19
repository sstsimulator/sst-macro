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

namespace redscat3 {

/** Limit the number of error reports */
#define MAX_ERRORS 10

int redscat3( int argc, char **argv )
{
    int      err = 0;
    int      *sendbuf, *recvbuf, *recvcounts;
    int      size, rank, i, j, idx, mycount, sumval;
    MPI_Comm comm;


    MTest_Init( &argc, &argv );
    comm = MPI_COMM_WORLD;

    MPI_Comm_size( comm, &size );
    MPI_Comm_rank( comm, &rank );
    recvcounts = (int *)malloc( size * sizeof(int) );
    if (!recvcounts) {
	fprintf( stderr, "Could not allocate %d ints for recvcounts\n", 
		 size );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    mycount = (1024 * 1024) / size;
    for (i=0; i<size; i++) 
	recvcounts[i] = mycount;
    sendbuf = (int *) malloc( mycount * size * sizeof(int) );
    if (!sendbuf) {
	fprintf( stderr, "Could not allocate %d ints for sendbuf\n", 
		 mycount * size );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    idx = 0;
    for (i=0; i<size; i++) {
	for (j=0; j<mycount; j++) {
	    sendbuf[idx++] = rank + i;
	}
    }
    recvbuf = (int *)malloc( mycount * sizeof(int) );
    if (!recvbuf) {
	fprintf( stderr, "Could not allocate %d ints for recvbuf\n", 
		 mycount );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    for (i=0; i<mycount; i++) {
	recvbuf[i] = -1;
    }

    MPI_Reduce_scatter( sendbuf, recvbuf, recvcounts, MPI_INT, MPI_SUM, comm );

    sumval = size * rank + ((size - 1) * size)/2;
    /** recvbuf should be size * (rank + i) */
    for (i=0; i<mycount; i++) {
	if (recvbuf[i] != sumval) {
	    err++;
	    if (err < MAX_ERRORS) {
		fprintf( stdout, "Did not get expected value for reduce scatter\n" );
		fprintf( stdout, "[%d] Got recvbuf[%d] = %d expected %d\n",
			 rank, i, recvbuf[i], sumval );
	    }
	}
    }

    MPI_Reduce_scatter( MPI_IN_PLACE, sendbuf, recvcounts, MPI_INT, MPI_SUM, 
			comm );

    sumval = size * rank + ((size - 1) * size)/2;
    /** recv'ed values for my process should be size * (rank + i) */
    for (i=0; i<mycount; i++) {
	if (sendbuf[i] != sumval) {
	    err++;
	    if (err < MAX_ERRORS) {
		fprintf( stdout, "Did not get expected value for reduce scatter (in place)\n" );
		fprintf( stdout, "[%d] Got buf[%d] = %d expected %d\n", 
			 rank, i, sendbuf[rank*mycount+i], sumval );
	    }
	}
    }

    free(sendbuf);
    free(recvbuf);
    free(recvcounts);
       
    MTest_Finalize( err );

    MPI_Finalize( );

    return 0;
}

}