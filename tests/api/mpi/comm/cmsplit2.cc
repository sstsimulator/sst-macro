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

namespace cmsplit2 {

#define ERRLIMIT (10)

#define my_assert(cond_)                                     \
    do {                                                     \
        if (!(cond_)) {                                      \
            if (errs < ERRLIMIT)                             \
                printf("assertion \"%s\" failed\n", #cond_); \
            ++errs;                                          \
        }                                                    \
    } while (0)

int cmsplit2(int argc, char **argv)
{
    int i, j, pos, modulus, cs, rank, size;
    int wrank, wsize;
    int newrank, newsize;
    int errs = 0;
    int key;
    int *oldranks = NULL;
    int *identity = NULL;
    int verbose = 0;
    MPI_Comm comm, splitcomm;
    MPI_Group wgroup, newgroup;

    MPI_Init(&argc, &argv);

    if (getenv("MPITEST_VERBOSE"))
        verbose = 1;

    MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    MPI_Comm_size(MPI_COMM_WORLD, &wsize);

    oldranks = (int*)malloc(wsize * sizeof(int));
    identity = (int*)malloc(wsize * sizeof(int));
    for (i = 0; i < wsize; ++i) {
        identity[i] = i;
    }

    for (cs = 1; cs <= wsize; ++cs) {
        /** yes, we are using comm_split to test comm_split, but this test is
         * mainly about ensuring that the stable sort behavior is correct, not
         * about whether the partitioning by color behavior is correct */
        MPI_Comm_split(MPI_COMM_WORLD, (wrank < cs ? 0 : MPI_UNDEFINED), wrank, &comm);
        if (comm != MPI_COMM_NULL) {
            MPI_Comm_rank(comm, &rank);
            MPI_Comm_size(comm, &size);

            for (modulus = 1; modulus <= size; ++modulus) {
                /** Divide all ranks into one of "modulus" equivalence classes.  Ranks in
                 * output comm will be ordered first by class, then within the class by
                 * rank in comm world. */
                key = rank % modulus;

                /** all pass same color, variable keys */
                MPI_Comm_split(comm, 5, key, &splitcomm);
                MPI_Comm_rank(splitcomm, &newrank);
                MPI_Comm_size(splitcomm, &newsize);
                my_assert(newsize == size);

                MPI_Comm_group(MPI_COMM_WORLD, &wgroup);
                MPI_Comm_group(splitcomm, &newgroup);
                int gsize;
                MPI_Group_size(newgroup, &gsize);
                MPI_Group_translate_ranks(newgroup, size, identity, wgroup, oldranks);
               // MPI_Group_free(&wgroup);
                MPI_Group_free(&newgroup);

                if (splitcomm != MPI_COMM_NULL)
                    MPI_Comm_free(&splitcomm);

                /** now check that comm_split broke any ties correctly */
                if (rank == 0) {
                    if (verbose) {
                        /** debugging code that is useful when the test fails */
                        printf("modulus=%d oldranks={", modulus);
                        for (i = 0; i < size - 1; ++i) {
                            printf("%d,", oldranks[i]);
                        }
                        printf("%d} keys={", oldranks[i]);
                        for (i = 0; i < size - 1; ++i) {
                            printf("%d,", i % modulus);
                        }
                        printf("%d}\n", i % modulus);
                    }

                    pos = 0;
                    for (i = 0; i < modulus; ++i) {
                        /** there's probably a better way to write these loop bounds and
                         * indices, but this is the first (correct) way that occurred to me */
                        for (j = 0; j < (size / modulus + (i < size % modulus ? 1 : 0)); ++j) {
                            if (errs < ERRLIMIT && oldranks[pos] != i+modulus*j) {
                                printf("size=%d i=%d j=%d modulus=%d pos=%d i+modulus*j=%d oldranks[pos]=%d\n",
                                       size, i, j, modulus, pos, i+modulus*j, oldranks[pos]);
                            }
                            my_assert(oldranks[pos] == i+modulus*j);
                            ++pos;
                        }
                    }
                }
            }
            MPI_Comm_free(&comm);
        }
    }

    if (oldranks != NULL)
        free(oldranks);
    if (identity != NULL)
        free(identity);

    if (rank == 0) {
        if (errs)
            printf("found %d Errors\n", errs);
        else
            printf(" No Errors\n");
    }

    MPI_Finalize();
    return 0;
}

}