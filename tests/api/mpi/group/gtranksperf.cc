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

#include <math.h> /** for fabs(3) */
#ifdef HAVE_UNISTD_H
#include <unistd.h> /** for sleep(3) */
#endif

namespace gtranksperf {

/** Measure and compare the relative performance of MPI_Group_translate_ranks
 * with small and large group2 sizes but a constant number of ranks.  This
 * serves as a performance sanity check for the Scalasca use case where we
 * translate to MPI_COMM_WORLD ranks.  The performance should only depend on the
 * number of ranks passed, not the size of either group (especially group2).
 *
 * This test is probably only meaningful for large-ish process counts, so we may
 * not be able to run this test by default in the nightlies. */

/** number of iterations used for timing */
#define NUM_LOOPS (1000000)

int gtranksperf( int argc, char *argv[] )
{
    int errs = 0;
    int *ranks;
    int *ranksout;
    MPI_Group gworld, grev, gself;
    MPI_Comm  comm;
    MPI_Comm  commrev;
    int rank, size, i;
    double start, end, time1, time2;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    MPI_Comm_size( comm, &size );
    MPI_Comm_rank( comm, &rank );

    ranks    = (int*)malloc(size*sizeof(int));
    ranksout = (int*)malloc(size*sizeof(int));
    if (!ranks || !ranksout) {
        fprintf(stderr, "out of memory\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /** generate a comm with the rank order reversed */
    MPI_Comm_split(comm, 0, (size-rank-1), &commrev);
    MPI_Comm_group(commrev, &grev);
    MPI_Comm_group(MPI_COMM_SELF, &gself);
    MPI_Comm_group(comm, &gworld);

    /** sanity check correctness first */
    for (i=0; i < size; i++) {
        ranks[i] = i;
        ranksout[i] = -1;
    }
    MPI_Group_translate_ranks(grev, size, ranks, gworld, ranksout);
    for (i=0; i < size; i++) {
        if (ranksout[i] != (size-i-1)) {
            if (rank == 0)
                printf("%d: (gworld) expected ranksout[%d]=%d, got %d\n", rank, i, (size-rank-1), ranksout[i]);
            ++errs;
        }
    }
    MPI_Group_translate_ranks(grev, size, ranks, gself, ranksout);
    for (i=0; i < size; i++) {
        int expected = (i == (size-rank-1) ? 0 : MPI_UNDEFINED);
        if (ranksout[i] != expected) {
            if (rank == 0)
                printf("%d: (gself) expected ranksout[%d]=%d, got %d\n", rank, i, expected, ranksout[i]);
            ++errs;
        }
    }

    /** now compare relative performance */

    /** we needs lots of procs to get a group large enough to have meaningful
     * numbers.  On most testing machines this means that we're oversubscribing
     * cores in a big way, which might perturb the timing results.  So we make
     * sure everyone started up and then everyone but rank 0 goes to sleep to
     * let rank 0 do all the timings. */
    MPI_Barrier(comm);

    if (rank != 0) {
        sleep(10);
    }
    else /** rank==0 */ {
        sleep(1); /** try to avoid timing while everyone else is making syscalls */

        MPI_Group_translate_ranks(grev, size, ranks, gworld, ranksout); /**throwaway iter*/
        start = MPI_Wtime();
        for (i = 0; i < NUM_LOOPS; ++i) {
            MPI_Group_translate_ranks(grev, size, ranks, gworld, ranksout);
        }
        end = MPI_Wtime();
        time1 = end - start;

        MPI_Group_translate_ranks(grev, size, ranks, gself, ranksout); /**throwaway iter*/
        start = MPI_Wtime();
        for (i = 0; i < NUM_LOOPS; ++i) {
            MPI_Group_translate_ranks(grev, size, ranks, gself, ranksout);
        }
        end = MPI_Wtime();
        time2 = end - start;

        /** complain if the "gworld" time exceeds 2x the "gself" time */
        if (fabs(time1 - time2) > (2.00 * time2)) {
            printf("too much difference in MPI_Group_translate_ranks performance:\n");
            printf("time1=%f time2=%f\n", time1, time2);
            printf("(fabs(time1-time2)/time2)=%f\n", (fabs(time1-time2)/time2));
            if (time1 < time2) {
                printf("also, (time1<time2) is surprising...\n");
            }
            ++errs;
        }
    }

    free(ranks);
    free(ranksout);

    MPI_Group_free(&grev);
    MPI_Group_free(&gself);
    MPI_Group_free(&gworld);

    MPI_Comm_free(&commrev);

    MTest_Finalize(errs);
    MPI_Finalize();

    return 0;
}

}