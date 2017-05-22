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
#include <stdlib.h>
#include <sstmac/replacements/mpi.h>
/** USE_STRICT_MPI may be defined in mpitestconf.h */
#include "mpitestconf.h"

namespace comm_group_rand {
#define LOOPS 100

int comm_group_rand(int argc, char **argv)
{
    int rank, size, i, j, count;
    MPI_Group full_group, sub_group;
    int *included, *ranks;
    MPI_Comm comm;

    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    ranks = (int*)malloc(size * sizeof(int));
    included = (int*)malloc(size * sizeof(int));
    MPI_Comm_group(MPI_COMM_WORLD, &full_group);

    for (j = 0; j < LOOPS; j++) {
        srand(j); /** Deterministic seed */

        count = 0;
        for (i = 0; i < size; i++) {
            if (rand() % 2) { /** randomly include a rank */
                included[i] = 1;
                ranks[count++] = i;
            }
            else
                included[i] = 0;
        }

        MPI_Group_incl(full_group, count, ranks, &sub_group);

#if !defined(USE_STRICT_MPI) && defined(MPICH2)
        if (included[rank]) {
            MPIX_Comm_create_group(MPI_COMM_WORLD, sub_group, 0, &comm);
            MPI_Barrier(comm);
            MPI_Comm_free(&comm);
        }
#endif /** USE_STRICT_MPI */

        MPI_Group_free(&sub_group);
    }

    MPI_Group_free(&full_group);

    if (rank == 0)
        printf(" No Errors\n");

    MPI_Finalize();

    return 0;
}

}