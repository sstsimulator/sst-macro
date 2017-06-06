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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mpitest.h"

namespace bcasttest {

#define ROOT      0
#define NUM_REPS  5
#define NUM_SIZES 4

int bcasttest( int argc, char **argv)
{
    int *buf;
    int i, rank, reps, n;
    int bVerify = 1;
    int sizes[NUM_SIZES] = { 100, 64*1024, 128*1024, 1024*1024 };
    int num_errors=0;
    
    MTest_Init( &argc, &argv );
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (argc > 1)
    {
	if (strcmp(argv[1], "-novalidate") == 0 || strcmp(argv[1], "-noverify") == 0)
	    bVerify = 0;
    }

    buf = (int *) malloc(sizes[NUM_SIZES-1]*sizeof(int));
    memset(buf, 0, sizes[NUM_SIZES-1]*sizeof(int));

    for (n=0; n<NUM_SIZES; n++)
    {
#ifdef DEBUG
	if (rank == ROOT)
	{
	    printf("bcasting %d MPI_INTs %d times\n", sizes[n], NUM_REPS);
	    fflush(stdout);
	}
#endif
	for (reps=0; reps < NUM_REPS; reps++)
	{
	    if (bVerify)
	    {
                if (rank == ROOT)
                {
		    for (i=0; i<sizes[n]; i++)
		    {
			buf[i] = 1000000 * (n * NUM_REPS + reps) + i;
		    }
		}
		else
                {
		    for (i=0; i<sizes[n]; i++)
		    {
                        buf[i] = -1 - (n * NUM_REPS + reps);
		    }
		}
	    }

#	    ifdef DEBUG
	    {
		printf("rank=%d, n=%d, reps=%d\n", rank, n, reps);
	    }
#           endif
	    
	    MPI_Bcast(buf, sizes[n], MPI_INT, ROOT, MPI_COMM_WORLD);

	    if (bVerify)
	    {
	        num_errors = 0;
		for (i=0; i<sizes[n]; i++)
		{
		    if (buf[i] != 1000000 * (n * NUM_REPS + reps) + i)
		    {
		        num_errors++;
			if (num_errors < 10)
			{
			    printf("Error: Rank=%d, n=%d, reps=%d, i=%d, buf[i]=%d expected=%d\n", rank, n, reps, i, buf[i],
				   1000000 * (n * NUM_REPS + reps) +i);
			    fflush(stdout);
			}
		    }
		}
		if (num_errors >= 10)
		{
		    printf("Error: Rank=%d, num_errors = %d\n", rank, num_errors);
		    fflush(stdout);
		}
	    }
	}
    }
    
    free(buf);

    MTest_Finalize( num_errors );
    MPI_Finalize();
    return 0;
}

}