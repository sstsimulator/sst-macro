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

namespace gtranks {
#define MAX_WORLD_SIZE 1024

int gtranks( int argc, char *argv[] )
{
    int errs = 0;
    int ranks[MAX_WORLD_SIZE], ranksout[MAX_WORLD_SIZE], 
	ranksin[MAX_WORLD_SIZE];
    int range[1][3];
    MPI_Group gworld, gself, ngroup, galt;
    MPI_Comm  comm;
    int rank, size, i, nelms;

    MTest_Init( &argc, &argv );

    MPI_Comm_group( MPI_COMM_SELF, &gself );

    comm = MPI_COMM_WORLD;

    MPI_Comm_size( comm, &size );
    MPI_Comm_rank( comm, &rank );

    if (size > MAX_WORLD_SIZE) {
	fprintf( stderr, 
	 "This test requires a comm world with no more than %d processes\n", 
		 MAX_WORLD_SIZE );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    if (size < 4) {
	fprintf( stderr, "This test requiers at least 4 processes\n" );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    MPI_Comm_group( comm, &gworld );
    for (i=0; i<size; i++) {
	ranks[i] = i;
	ranksout[i] = -1;
    }
    /** Try translating ranks from comm world compared against
       comm self, so most will be UNDEFINED */
    MPI_Group_translate_ranks( gworld, size, ranks, gself, ranksout );
    
    for (i=0; i<size; i++) {
	if (i == rank) {
	    if (ranksout[i] != 0) {
		printf( "[%d] Rank %d is %d but should be 0\n", rank, 
			i, ranksout[i] );
		errs++;
	    }
	}
	else {
	    if (ranksout[i] != MPI_UNDEFINED) {
		printf( "[%d] Rank %d is %d but should be undefined\n", rank, 
			i, ranksout[i] );
		errs++;
	    }
	}
    }

    /** MPI-2 Errata requires that MPI_PROC_NULL is mapped to MPI_PROC_NULL */
    ranks[0] = MPI_PROC_NULL;
    ranks[1] = 1;
    ranks[2] = rank;
    ranks[3] = MPI_PROC_NULL;
    for (i=0; i<4; i++) ranksout[i] = -1;

    MPI_Group_translate_ranks( gworld, 4, ranks, gself, ranksout );
    if (ranksout[0] != MPI_PROC_NULL) {
	printf( "[%d] Rank[0] should be MPI_PROC_NULL but is %d\n",
		rank, ranksout[0] );
	errs++;
    }
    if (rank != 1 && ranksout[1] != MPI_UNDEFINED) {
	printf( "[%d] Rank[1] should be MPI_UNDEFINED but is %d\n",
		rank, ranksout[1] );
	errs++;
    }
    if (rank == 1 && ranksout[1] != 0) {
	printf( "[%d] Rank[1] should be 0 but is %d\n",
		rank, ranksout[1] );
	errs++;
    }
    if (ranksout[2] != 0) {
	printf( "[%d] Rank[2] should be 0 but is %d\n",
		rank, ranksout[2] );
	errs++;
    }
    if (ranksout[3] != MPI_PROC_NULL) {
	printf( "[%d] Rank[3] should be MPI_PROC_NULL but is %d\n",
		rank, ranksout[3] );
	errs++;
    }

    MPI_Group_free(&gself);

    /** Now, try comparing small groups against larger groups, and use groups
       with irregular members (to bypass optimizations in group_translate_ranks
       for simple groups)
     */
    nelms = 0;
    ranks[nelms++] = size - 2;
    ranks[nelms++] = 0;
    if (rank != 0 && rank != size - 2) {
	ranks[nelms++] = rank; 
    }

    MPI_Group_incl( gworld, nelms, ranks, &ngroup );

    for (i=0; i<nelms; i++) ranksout[i] = -1;
    ranksin[0] = 1;
    ranksin[1] = 0;
    ranksin[2] = MPI_PROC_NULL;
    ranksin[3] = 2;
    MPI_Group_translate_ranks( ngroup, nelms+1, ranksin, gworld, ranksout );
    for (i=0; i<nelms+1; i++) {
	if (ranksin[i] == MPI_PROC_NULL) {
	    if (ranksout[i] != MPI_PROC_NULL) {
		fprintf( stderr, "Input rank for proc_null but output was %d\n",
			 ranksout[i] );
		errs++;
	    }
	}
	else if (ranksout[i] != ranks[ranksin[i]]) {
	    fprintf( stderr, "Expected ranksout[%d] = %d but found %d\n",
		     i, ranks[ranksin[i]], ranksout[i] );
	    errs++;
	}
    }
    
    range[0][0] = size -1 ;
    range[0][1] = 0;
    range[0][2] = -1;
    MPI_Group_range_incl( gworld, 1, range, &galt);
    for (i=0; i<nelms+1; i++) ranksout[i] = -1;
    MPI_Group_translate_ranks( ngroup, nelms+1, ranksin, galt, ranksout );
    for (i=0; i<nelms+1; i++) {
	if (ranksin[i] == MPI_PROC_NULL) {
	    if (ranksout[i] != MPI_PROC_NULL) {
		fprintf( stderr, "Input rank for proc_null but output was %d\n",
			 ranksout[i] );
		errs++;
	    }
	}
	else if (ranksout[i] != (size-1)-ranks[ranksin[i]]) {
	    fprintf( stderr, "Expected ranksout[%d] = %d but found %d\n",
		     i, (size-1)-ranks[ranksin[i]], ranksout[i] );
	    errs++;
	}
    }
    
    
    MPI_Group_free(&gworld);
    MPI_Group_free(&galt);
    MPI_Group_free(&ngroup);

    MTest_Finalize( errs );
    MPI_Finalize();

    return 0;
}

}