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

namespace ic2 {

int ic2(int argc, char **argv)
{
    MPI_Comm c0, c1, ic;
    MPI_Group g0, g1, gworld;
    int a, b, c, d;
    int rank, size, remote_leader, tag;
    int ranks[2];
    int errs = 0;

    tag = 5;
    c0 = c1 = ic = MPI_COMM_NULL;
    g0 = g1 = gworld = MPI_GROUP_NULL;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 33) {
        printf("ERROR: this test requires at least 33 processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
        return 1;
    }

    /** group of c0
     * NOTE: a>=32 is essential for exercising the loop bounds bug from tt#1574 */
    a = 32;
    b = 24;

    /** group of c1 */
    c = 25;
    d = 26;

    MPI_Comm_group(MPI_COMM_WORLD, &gworld);

    ranks[0] = a;
    ranks[1] = b;
    MPI_Group_incl(gworld, 2, ranks, &g0);
    MPI_Comm_create(MPI_COMM_WORLD, g0, &c0);

    ranks[0] = c;
    ranks[1] = d;
    MPI_Group_incl(gworld, 2, ranks, &g1);
    MPI_Comm_create(MPI_COMM_WORLD, g1, &c1);

    if (rank == a || rank == b) {
        remote_leader = c;
        MPI_Intercomm_create(c0, 0, MPI_COMM_WORLD, remote_leader, tag, &ic);
    }
    else if (rank == c || rank == d) {
        remote_leader = a;
        MPI_Intercomm_create(c1, 0, MPI_COMM_WORLD, remote_leader, tag, &ic);
    }

    MPI_Group_free(&g0);
    MPI_Group_free(&g1);
    MPI_Group_free(&gworld);

    if (c0 != MPI_COMM_NULL)
        MPI_Comm_free(&c0);
    if (c1 != MPI_COMM_NULL)
        MPI_Comm_free(&c1);
    if (ic != MPI_COMM_NULL)
        MPI_Comm_free(&ic);


    MPI_Reduce((rank == 0 ? MPI_IN_PLACE : &errs), &errs,
               1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
    if (rank == 0) {
        if (errs) {
            printf("found %d errors\n", errs);
        }
        else {
            printf(" No errors\n");
        }
    }
    MPI_Finalize();

    return 0;
}

}