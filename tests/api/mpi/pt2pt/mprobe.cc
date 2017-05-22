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

namespace mprobe {

/** This is a temporary #ifdef to control whether we test this functionality.  A
 * configure-test or similar would be better.  Eventually the MPI-3 standard
 * will be released and this can be gated on a MPI_VERSION check */
#if !defined(USE_STRICT_MPI) && defined(MPICH2)
#define TEST_MPROBE_ROUTINES 1
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

int mprobe(int argc, char **argv)
{
    int errs = 0;
    int found, completed;
    int rank, size;
    int sendbuf[8], recvbuf[8];
    int count;
#ifdef TEST_MPROBE_ROUTINES
    MPIX_Message msg;
#endif
    MPI_Request rreq;
    MPI_Status s1, s2;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 2) {
        printf("this test requires at least 2 processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /** all processes besides ranks 0 & 1 aren't used by this test */
    if (rank >= 2) {
        goto epilogue;
    }

#ifdef TEST_MPROBE_ROUTINES
    /** test 0: simple send & mprobe+mrecv */
    if (rank == 0) {
        sendbuf[0] = 0xdeadbeef;
        sendbuf[1] = 0xfeedface;
        MPI_Send(sendbuf, 2, MPI_INT, 1, 5, MPI_COMM_WORLD);
    }
    else {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        MPIX_Mprobe(0, 5, MPI_COMM_WORLD, &msg, &s1);
        check(s1.MPI_SOURCE == 0);
        check(s1.MPI_TAG == 5);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);
        check(msg != MPIX_MESSAGE_NULL);

        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 2);

        recvbuf[0] = 0x01234567;
        recvbuf[1] = 0x89abcdef;
        MPIX_Mrecv(recvbuf, count, MPI_INT, &msg, &s2);
        check(recvbuf[0] == 0xdeadbeef);
        check(recvbuf[1] == 0xfeedface);
        check(s2.MPI_SOURCE == 0);
        check(s2.MPI_TAG == 5);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
    }

    /** test 1: simple send & mprobe+imrecv */
    if (rank == 0) {
        sendbuf[0] = 0xdeadbeef;
        sendbuf[1] = 0xfeedface;
        MPI_Send(sendbuf, 2, MPI_INT, 1, 5, MPI_COMM_WORLD);
    }
    else {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        MPIX_Mprobe(0, 5, MPI_COMM_WORLD, &msg, &s1);
        check(s1.MPI_SOURCE == 0);
        check(s1.MPI_TAG == 5);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);
        check(msg != MPIX_MESSAGE_NULL);

        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 2);

        rreq = MPI_REQUEST_NULL;
        recvbuf[0] = 0x01234567;
        recvbuf[1] = 0x89abcdef;
        MPIX_Imrecv(recvbuf, count, MPI_INT, &msg, &rreq);
        check(rreq != MPI_REQUEST_NULL);
        MPI_Wait(&rreq, &s2);
        check(recvbuf[0] == 0xdeadbeef);
        check(recvbuf[1] == 0xfeedface);
        check(s2.MPI_SOURCE == 0);
        check(s2.MPI_TAG == 5);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
    }

    /** test 2: simple send & improbe+mrecv */
    if (rank == 0) {
        sendbuf[0] = 0xdeadbeef;
        sendbuf[1] = 0xfeedface;
        MPI_Send(sendbuf, 2, MPI_INT, 1, 5, MPI_COMM_WORLD);
    }
    else {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        do {
            check(msg == MPIX_MESSAGE_NULL);
            MPIX_Improbe(0, 5, MPI_COMM_WORLD, &found, &msg, &s1);
        } while (!found);
        check(msg != MPIX_MESSAGE_NULL);
        check(s1.MPI_SOURCE == 0);
        check(s1.MPI_TAG == 5);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);

        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 2);

        recvbuf[0] = 0x01234567;
        recvbuf[1] = 0x89abcdef;
        MPIX_Mrecv(recvbuf, count, MPI_INT, &msg, &s2);
        check(recvbuf[0] == 0xdeadbeef);
        check(recvbuf[1] == 0xfeedface);
        check(s2.MPI_SOURCE == 0);
        check(s2.MPI_TAG == 5);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
    }

    /** test 3: simple send & improbe+imrecv */
    if (rank == 0) {
        sendbuf[0] = 0xdeadbeef;
        sendbuf[1] = 0xfeedface;
        MPI_Send(sendbuf, 2, MPI_INT, 1, 5, MPI_COMM_WORLD);
    }
    else {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        do {
            check(msg == MPIX_MESSAGE_NULL);
            MPIX_Improbe(0, 5, MPI_COMM_WORLD, &found, &msg, &s1);
        } while (!found);
        check(msg != MPIX_MESSAGE_NULL);
        check(s1.MPI_SOURCE == 0);
        check(s1.MPI_TAG == 5);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);

        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 2);

        rreq = MPI_REQUEST_NULL;
        MPIX_Imrecv(recvbuf, count, MPI_INT, &msg, &rreq);
        check(rreq != MPI_REQUEST_NULL);
        MPI_Wait(&rreq, &s2);
        check(recvbuf[0] == 0xdeadbeef);
        check(recvbuf[1] == 0xfeedface);
        check(s2.MPI_SOURCE == 0);
        check(s2.MPI_TAG == 5);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
    }

    /** test 4: mprobe+mrecv with MPI_PROC_NULL */
    {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        MPIX_Mprobe(MPI_PROC_NULL, 5, MPI_COMM_WORLD, &msg, &s1);
        check(s1.MPI_SOURCE == MPI_PROC_NULL);
        check(s1.MPI_TAG == MPI_ANY_TAG);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);
        check(msg == MPIX_MESSAGE_NO_PROC);

        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 0);

        recvbuf[0] = 0x01234567;
        recvbuf[1] = 0x89abcdef;
        MPIX_Mrecv(recvbuf, count, MPI_INT, &msg, &s2);
        /** recvbuf should remain unmodified */
        check(recvbuf[0] == 0x01234567);
        check(recvbuf[1] == 0x89abcdef);
        /** should get back "proc null status" */
        check(s2.MPI_SOURCE == MPI_PROC_NULL);
        check(s2.MPI_TAG == MPI_ANY_TAG);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
        count = -1;
        MPI_Get_count(&s2, MPI_INT, &count);
        check(count == 0);
    }

    /** test 5: mprobe+imrecv with MPI_PROC_NULL */
    {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        MPIX_Mprobe(MPI_PROC_NULL, 5, MPI_COMM_WORLD, &msg, &s1);
        check(s1.MPI_SOURCE == MPI_PROC_NULL);
        check(s1.MPI_TAG == MPI_ANY_TAG);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);
        check(msg == MPIX_MESSAGE_NO_PROC);
        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 0);

        rreq = MPI_REQUEST_NULL;
        recvbuf[0] = 0x01234567;
        recvbuf[1] = 0x89abcdef;
        MPIX_Imrecv(recvbuf, count, MPI_INT, &msg, &rreq);
        check(rreq != MPI_REQUEST_NULL);
        completed = 0;
        MPI_Test(&rreq, &completed, &s2); /** single test should always succeed */
        check(completed);
        /** recvbuf should remain unmodified */
        check(recvbuf[0] == 0x01234567);
        check(recvbuf[1] == 0x89abcdef);
        /** should get back "proc null status" */
        check(s2.MPI_SOURCE == MPI_PROC_NULL);
        check(s2.MPI_TAG == MPI_ANY_TAG);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
        count = -1;
        MPI_Get_count(&s2, MPI_INT, &count);
        check(count == 0);
    }

    /** test 6: improbe+mrecv with MPI_PROC_NULL */
    {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        found = 0;
        MPIX_Improbe(MPI_PROC_NULL, 5, MPI_COMM_WORLD, &found, &msg, &s1);
        check(found);
        check(msg == MPIX_MESSAGE_NO_PROC);
        check(s1.MPI_SOURCE == MPI_PROC_NULL);
        check(s1.MPI_TAG == MPI_ANY_TAG);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);
        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 0);

        recvbuf[0] = 0x01234567;
        recvbuf[1] = 0x89abcdef;
        MPIX_Mrecv(recvbuf, count, MPI_INT, &msg, &s2);
        /** recvbuf should remain unmodified */
        check(recvbuf[0] == 0x01234567);
        check(recvbuf[1] == 0x89abcdef);
        /** should get back "proc null status" */
        check(s2.MPI_SOURCE == MPI_PROC_NULL);
        check(s2.MPI_TAG == MPI_ANY_TAG);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
        count = -1;
        MPI_Get_count(&s2, MPI_INT, &count);
        check(count == 0);
    }

    /** test 7: improbe+imrecv */
    {
        memset(&s1, 0xab, sizeof(MPI_Status));
        memset(&s2, 0xab, sizeof(MPI_Status));
        /** the error field should remain unmodified */
        s1.MPI_ERROR = MPI_ERR_DIMS;
        s2.MPI_ERROR = MPI_ERR_TOPOLOGY;

        msg = MPIX_MESSAGE_NULL;
        MPIX_Improbe(MPI_PROC_NULL, 5, MPI_COMM_WORLD, &found, &msg, &s1);
        check(found);
        check(msg == MPIX_MESSAGE_NO_PROC);
        check(s1.MPI_SOURCE == MPI_PROC_NULL);
        check(s1.MPI_TAG == MPI_ANY_TAG);
        check(s1.MPI_ERROR == MPI_ERR_DIMS);
        count = -1;
        MPI_Get_count(&s1, MPI_INT, &count);
        check(count == 0);

        rreq = MPI_REQUEST_NULL;
        MPIX_Imrecv(recvbuf, count, MPI_INT, &msg, &rreq);
        check(rreq != MPI_REQUEST_NULL);
        completed = 0;
        MPI_Test(&rreq, &completed, &s2); /** single test should always succeed */
        check(completed);
        /** recvbuf should remain unmodified */
        check(recvbuf[0] == 0x01234567);
        check(recvbuf[1] == 0x89abcdef);
        /** should get back "proc null status" */
        check(s2.MPI_SOURCE == MPI_PROC_NULL);
        check(s2.MPI_TAG == MPI_ANY_TAG);
        check(s2.MPI_ERROR == MPI_ERR_TOPOLOGY);
        check(msg == MPIX_MESSAGE_NULL);
        count = -1;
        MPI_Get_count(&s2, MPI_INT, &count);
        check(count == 0);
    }

    /** TODO MPI_ANY_SOURCE and MPI_ANY_TAG should be tested as well */
    /** TODO a full range of message sizes should be tested too */
    /** TODO threaded tests are also needed, but they should go in a separate
     * program */

    /** simple test to ensure that c2f/f2c routines are present (initially missed
     * in MPICH2 impl) */
    {
        MPI_Fint f_handle = 0xdeadbeef;
        f_handle = MPIX_Message_c2f(MPIX_MESSAGE_NULL);
        msg = MPIX_Message_f2c(f_handle);
        check(f_handle != 0xdeadbeef);
        check(msg == MPIX_MESSAGE_NULL);

        /** PMPIX_ versions should also exists */
        f_handle = 0xdeadbeef;
        f_handle = PMPIX_Message_c2f(MPIX_MESSAGE_NULL);
        msg = PMPIX_Message_f2c(f_handle);
        check(f_handle != 0xdeadbeef);
        check(msg == MPIX_MESSAGE_NULL);
    }

#endif /** TEST_MPROBE_ROUTINES */

epilogue:
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