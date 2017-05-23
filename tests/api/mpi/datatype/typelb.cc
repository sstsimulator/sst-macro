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

namespace typelb {
int typelb( int argc, char **argv)
{
    int blockcnt[2], rank;
    MPI_Aint offsets[2], lb, ub, extent;
    MPI_Datatype tmp_type, newtype;

    MPI_Init(&argc, &argv);

    /** Set some values in locations that should not be accessed */
    blockcnt[1] = -1;
    offsets[1] = -1;

    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    if (rank == 0) {
	blockcnt[0] = 1;
	offsets[0] = 3;
	MPI_Type_hindexed(1, blockcnt, offsets, MPI_BYTE, &tmp_type);
	blockcnt[0] = 1;
	offsets[0] = 1;
	MPI_Type_hindexed(1, blockcnt, offsets, tmp_type, &newtype);
	MPI_Type_commit(&newtype);
	
	MPI_Type_lb(newtype, &lb);
	MPI_Type_extent(newtype, &extent);
	MPI_Type_ub(newtype, &ub);
	
	/** Check that the results are correct */
#ifdef DEBUG
	printf("lb=%ld, ub=%ld, extent=%ld\n", lb, ub, extent);
	printf("Should be lb=4, ub=5, extent=1\n");
#endif
	if (lb != 4 || ub != 5 || extent != 1) {
	  printf ("lb = %d (should be 4), ub = %d (should be 5) extent = %d should be 1\n", (int)lb, (int)ub, (int)extent) ;
	}
	else {
	    printf( " No Errors\n" );
	}

	MPI_Type_free(&tmp_type);
	MPI_Type_free(&newtype);
    }

    MPI_Finalize();
    return 0;
}

}