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

namespace grouptest {
int grouptest( int argc, char *argv[] )
{
    MPI_Group g1, g2, g4, g5, g45, selfgroup, g6;
    int ranks[16], size, rank, myrank, range[1][3];
    int errs = 0;
    int i, rin[16], rout[16], result;

    MPI_Init(0,0);

	MPI_Comm_group( MPI_COMM_WORLD, &g1 );
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );
	if (size < 8) {
	    fprintf( stderr, 
		  "Test requires 8 processes (16 prefered) only %d provided\n",
		     size );
	    errs++;
	}

	/** 16 members, this process is rank 0, return in group 1 */
	ranks[0] = myrank; ranks[1] = 2; ranks[2] = 7;
	if (myrank == 2) ranks[1] = 3;
	if (myrank == 7) ranks[2] = 6;
	MPI_Group_incl( g1, 3, ranks, &g2 );
	
	/** Check the resulting group */
	MPI_Group_size( g2, &size );
	MPI_Group_rank( g2, &rank );
	
	if (size != 3) {
	    fprintf( stderr, "Size should be %d, is %d\n", 3, size );
	    errs++;
	}
	if (rank != 0) {
	    fprintf( stderr, "Rank should be %d, is %d\n", 0, rank );
	    errs++;
	}

	rin[0] = 0; rin[1] = 1; rin[2] = 2;
	MPI_Group_translate_ranks( g2, 3, rin, g1, rout );
	for (i=0; i<3; i++) {
	    if (rout[i] != ranks[i]) {
		fprintf( stderr, "translated rank[%d] %d should be %d\n", 
			 i, rout[i], ranks[i] );
		errs++;
	    }
	}
	
	/** Translate the process of the self group against another group */
	MPI_Comm_group( MPI_COMM_SELF, &selfgroup );
	rin[0] = 0;
	MPI_Group_translate_ranks( selfgroup, 1, rin, g1, rout );
	if (rout[0] != myrank) {
	    fprintf( stderr, "translated of self is %d should be %d\n", 
			 rout[0], myrank );
	    errs++;
	}

	for (i=0; i<size; i++) 
	    rin[i] = i;
	MPI_Group_translate_ranks( g1, size, rin, selfgroup, rout );
	for (i=0; i<size; i++) {
	    if (i == myrank && rout[i] != 0) {
		fprintf( stderr, "translated world to self of %d is %d\n",
			 i, rout[i] );
		errs++;
	    }
	    else if (i != myrank && rout[i] != MPI_UNDEFINED) {
		fprintf( stderr, "translated world to self of %d should be undefined, is %d\n",
			 i, rout[i] );
		errs++;
	    }
	}
	MPI_Group_free( &selfgroup );

	/** Exclude everyone in our group */
	{
	    int ii, *lranks, g1size;

	    MPI_Group_size( g1, &g1size );
	    
	    lranks = (int *)malloc( g1size * sizeof(int) );
	    for (ii=0; ii<g1size; ii++) lranks[ii] = ii;
	    MPI_Group_excl( g1, g1size, lranks, &g6 );
	    if (g6 != MPI_GROUP_EMPTY) {
		fprintf( stderr, "Group formed by excluding all ranks not empty\n" );
		errs++;
		MPI_Group_free( &g6 );
	    }
	    free( lranks );
	}
	
	/** Add tests for additional group operations */
	/** 
	   g2 = incl 1,3,7
	   g3 = excl 1,3,7
	   intersect ( w, g2 ) => g2
	   intersect ( w, g3 ) => g3
	   intersect ( g2, g3 ) => empty
	   
	   g4 = rincl 1:n-1:2
	   g5 = rexcl 1:n-1:2
	   union( g4, g5 ) => world
	   g6 = rincl n-1:1:-1 
	   g7 = rexcl n-1:1:-1
	   union( g6, g7 ) => concat of entries, similar to world
	   diff( w, g2 ) => g3
	*/
	MPI_Group_free( &g2 );

	range[0][0] = 1;
	range[0][1] = size-1;
	range[0][2] = 2;
	MPI_Group_range_excl( g1, 1, range, &g5 );

	range[0][0] = 1;
	range[0][1] = size-1;
	range[0][2] = 2;
	MPI_Group_range_incl( g1, 1, range, &g4 );

	MPI_Group_union( g4, g5, &g45 );

	MPI_Group_compare( MPI_GROUP_EMPTY, g4, &result );
	if (result != MPI_UNEQUAL) {
	    errs++;
	    fprintf( stderr, "Comparison with empty group gave %d, not 3\n",
		     result );
	}
	MPI_Group_free( &g4 );
	MPI_Group_free( &g5 );
	MPI_Group_free( &g45 );

	/** Now, duplicate the test, but using negative strides */
	range[0][0] = size-1;
	range[0][1] = 1;
	range[0][2] = -2;
	MPI_Group_range_excl( g1, 1, range, &g5 );

	range[0][0] = size-1;
	range[0][1] = 1;
	range[0][2] = -2;
	MPI_Group_range_incl( g1, 1, range, &g4 );

	MPI_Group_union( g4, g5, &g45 );

	MPI_Group_compare( MPI_GROUP_EMPTY, g4, &result );
	if (result != MPI_UNEQUAL) {
	    errs++;
	    fprintf( stderr, "Comparison with empty group (formed with negative strides) gave %d, not 3\n",
		     result );
	}
	MPI_Group_free( &g4 );
	MPI_Group_free( &g5 );
	MPI_Group_free( &g45 );
        MPI_Group_free( &g1 );

    if (myrank == 0) 
    {
	if (errs == 0) {
	    printf( " No Errors\n" );
	}
	else {
	    printf( "Found %d errors\n", errs );
	}
    }

    MPI_Finalize();
    return 0;
}

}