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
#include <string.h>
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace comm_idup {

/** This is a temporary #ifdef to control whether we test this functionality.  A
 * configure-test or similar would be better.  Eventually the MPI-3 standard
 * will be released and this can be gated on a MPI_VERSION check */
#if !defined(USE_STRICT_MPI) && defined(MPICH2)
#define TEST_IDUP 1
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

int comm_idup(int argc, char **argv)
{
    int errs = 0;
    int i;
    int rank, size, lrank, lsize, rsize;
    int buf[2];
    MPI_Comm newcomm, ic, localcomm;
    MPI_Request rreq;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 2) {
        printf("this test requires at least 2 processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

#ifdef TEST_IDUP

    /** test plan: make rank 0 wait in a blocking recv until all other processes
     * have posted their MPIX_Comm_idup ops, then post last.  Should ensure that
     * idup doesn't block on the non-zero ranks, otherwise we'll get a deadlock.
     */

    if (rank == 0) {
        for (i = 1; i < size; ++i) {
            buf[0] = 0x01234567;
            buf[1] = 0x89abcdef;
            MPI_Recv(buf, 2, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
        MPIX_Comm_idup(MPI_COMM_WORLD, &newcomm, &rreq);
        MPI_Wait(&rreq, MPI_STATUS_IGNORE);
    }
    else {
        MPIX_Comm_idup(MPI_COMM_WORLD, &newcomm, &rreq);
        buf[0] = rank;
        buf[1] = size + rank;
        MPI_Ssend(buf, 2, MPI_INT, 0, 0, MPI_COMM_WORLD);
        MPI_Wait(&rreq, MPI_STATUS_IGNORE);
    }

    /** do some communication to make sure that newcomm works */
    buf[0] = rank;
    buf[1] = 0xfeedface;
    MPI_Allreduce(&buf[0], &buf[1], 1, MPI_INT, MPI_SUM, newcomm);
    check(buf[1] == (size * (size-1) / 2));

    MPI_Comm_free(&newcomm);

    /** now construct an intercomm and make sure we can dup that too */
    MPI_Comm_split(MPI_COMM_WORLD, rank % 2, rank, &localcomm);
    MPI_Intercomm_create(localcomm, 0, MPI_COMM_WORLD, (rank == 0 ? 1 : 0), 1234, &ic);
    MPI_Comm_free(&localcomm);

    MPI_Comm_rank(ic, &lrank);
    MPI_Comm_size(ic, &lsize);
    MPI_Comm_remote_size(ic, &rsize);

    /** Similar to above pattern, but all non-local-rank-0 processes send to
     * remote rank 0.  Both sides participate in this way. */
    if (lrank == 0) {
        for (i = 1; i < rsize; ++i) {
            buf[0] = 0x01234567;
            buf[1] = 0x89abcdef;
            MPI_Recv(buf, 2, MPI_INT, i, 0, ic, MPI_STATUS_IGNORE);
        }
        MPIX_Comm_idup(ic, &newcomm, &rreq);
        MPI_Wait(&rreq, MPI_STATUS_IGNORE);
    }
    else {
        MPIX_Comm_idup(ic, &newcomm, &rreq);
        buf[0] = lrank;
        buf[1] = lsize + lrank;
        MPI_Ssend(buf, 2, MPI_INT, 0, 0, ic);
        MPI_Wait(&rreq, MPI_STATUS_IGNORE);
    }

    /** do some communication to make sure that newcomm works */
    buf[0] = lrank;
    buf[1] = 0xfeedface;
    MPI_Allreduce(&buf[0], &buf[1], 1, MPI_INT, MPI_SUM, newcomm);
    check(buf[1] == (rsize * (rsize-1) / 2));

    MPI_Comm_free(&newcomm);
    MPI_Comm_free(&ic);

#endif /** TEST_IDUP */

    MPI_Reduce((rank == 0 ? MPI_IN_PLACE : &errs), &errs, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
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