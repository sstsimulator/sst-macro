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
/** USE_STRICT_MPI may be defined in mpitestconf.h */
#include "mpitestconf.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

namespace comm_create_group {

int comm_create_group(int argc, char *argv[])
{
    int size, rank, i, *excl;
    MPI_Group world_group, even_group;
    MPI_Comm even_comm;

    MPI_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (size % 2) {
        fprintf(stderr, "this program requires a multiple of 2 number of processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    excl = (int*)malloc((size / 2) * sizeof(int));
    assert(excl);

    /** exclude the odd ranks */
    for (i = 0; i < size / 2; i++)
        excl[i] = (2 * i) + 1;

    /** Create some groups */
    MPI_Comm_group(MPI_COMM_WORLD, &world_group);
    MPI_Group_excl(world_group, size / 2, excl, &even_group);
    MPI_Group_free(&world_group);

#if !defined(USE_STRICT_MPI) && defined(MPICH2)
    if (rank % 2 == 0) {
        /** Even processes create a group for themselves */
        MPIX_Comm_create_group(MPI_COMM_WORLD, even_group, 0, &even_comm);
        MPI_Barrier(even_comm);
        MPI_Comm_free(&even_comm);
    }
#endif /** USE_STRICT_MPI */

    MPI_Group_free(&even_group);
    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0)
        printf(" No Errors\n");

    MPI_Finalize();
    return 0;
}

}