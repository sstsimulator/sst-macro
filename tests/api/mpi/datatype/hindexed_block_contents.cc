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
#include "mpitestconf.h"

namespace hindexed_block_contents {


#if !defined(USE_STRICT_MPI) && defined(MPICH2)
#  define TEST_MPI3_ROUTINES 1
#endif

/** assert-like macro that bumps the err count and emits a message */
#define check(x_)                                                                 \
    do {                                                                          \
        if (!(x_)) {                                                              \
            ++errs;                                                               \
            if (errs < 10) {                                                      \
                fprintf(stderr, "check failed: (%s), line %d\n", #x_, __LINE__); \
            }                                                                     \
        }                                                                         \
    } while (0)

int hindexed_block_contents(int argc, char **argv)
{
    int errs = 0;
    int rank;
    MPI_Datatype t;
    int count = 4;
    int blocklength = 2;
    MPI_Aint displacements[] = {0, 8, 16, 24};

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

#ifdef TEST_MPI3_ROUTINES
    if (!rank) {
        MPIX_Type_create_hindexed_block(count, blocklength,
                                        displacements, MPI_INT,
                                        &t);
        MPI_Type_commit(&t);
        {
            int ni, na, nd, combiner;
            int i[1024];
            MPI_Aint a[1024];
            MPI_Datatype d[1024];
            int k;
            MPI_Type_get_envelope(t, &ni, &na, &nd, &combiner);
            MPI_Type_get_contents(t, ni, na, nd, i, a, d);

            check(ni == 2);
            check(i[0] == 4);
            check(i[1] == 2);

            check(na == 4);
            for (k=0; k < na; k++)
                check(a[k] == (k * 8));

            check(nd == 1);
            check(d[0] == MPI_INT);
        }

        MPI_Type_free(&t);
    }
#endif /** defined(TEST_MPI3_ROUTINES) */

    if (rank == 0) {
        if (errs) {
            printf("found %d errors\n", errs);
        }
        else {
            printf(" No Errors\n");
        }
    }
    MPI_Finalize();
    return 0;
}

}