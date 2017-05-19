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
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace attrend2 {

/** 20 ought to be enough attributes to ensure that hash-table based MPI
 * implementations do not accidentally pass the test except by being extremely
 * "lucky".  There are (20!) possible permutations which means that there is
 * about a 1 in 2.43e18 chance of getting LIFO ordering out of a hash table,
 * assuming a decent hash function is used. */
#define NUM_TEST_ATTRS (20)

static int exit_keys[NUM_TEST_ATTRS]; /** init to MPI_KEYVAL_INVALID */
static int was_called[NUM_TEST_ATTRS];
int foundError = 0;
int delete_fn (MPI_Comm, int, void *, void *);

int attrend2(int argc, char **argv)
{
    int errs = 0, wrank;
    int i;

    MTest_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &wrank);

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
    for (i = 0; i < NUM_TEST_ATTRS; ++i) {
        exit_keys[i] = MPI_KEYVAL_INVALID;
        was_called[i] = 0;

        /** create the keyval for the exit handler */
        MPI_Comm_create_keyval(MPI_COMM_NULL_COPY_FN, delete_fn, &exit_keys[i], NULL);
        /** attach to comm_self */
        MPI_Comm_set_attr(MPI_COMM_SELF, exit_keys[i], (void*)(long)i);
    }

    /** we can free the keys now */
    for (i = 0; i < NUM_TEST_ATTRS; ++i) {
        MPI_Comm_free_keyval(&exit_keys[i]);
    }

    /** now, exit MPI */
    MPI_Finalize();

    /** check that the exit handlers were called in LIFO order, and without error */
    if (wrank == 0) {
        /** In case more than one process exits MPI_Finalize */
        for (i = 0; i < NUM_TEST_ATTRS; ++i) {
            if (was_called[i] < 1) {
                errs++;
                printf("Attribute delete function on MPI_COMM_SELF was not called for idx=%d\n", i);
            }
            else if (was_called[i] > 1) {
                errs++;
                printf("Attribute delete function on MPI_COMM_SELF was called multiple times for idx=%d\n", i);
            }
        }
        if (foundError != 0) {
            errs++;
            printf("Found %d errors while executing delete function in MPI_COMM_SELF\n", foundError);
        }
        if (errs == 0) {
            printf(" No Errors\n");
        }
        else {
            printf(" Found %d errors\n", errs);
        }
        fflush(stdout);
    }
#else /** this is a pre-MPI-2.2 implementation, ordering is not defined */
    MPI_Finalize();
    if (wrank == 0)
        printf(" No Errors\n");
#endif

    return 0;
}

int delete_fn(MPI_Comm comm, int keyval, void *attribute_val, void *extra_state)
{
    int flag;
    int i;
    int my_idx = (int)(long)attribute_val;

    if (my_idx < 0 || my_idx > NUM_TEST_ATTRS) {
        printf("internal error, my_idx=%d is invalid!\n", my_idx);
        fflush(stdout);
    }

    was_called[my_idx]++;

    MPI_Finalized(&flag);
    if (flag) {
        printf("my_idx=%d, MPI_Finalized returned %d, should have been 0", my_idx, flag);
        foundError++;
    }

    /** since attributes were added in 0..(NUM_TEST_ATTRS-1) order, they will be
     * called in (NUM_TEST_ATTRS-1)..0 order */
    for (i = 0; i < my_idx; ++i) {
        if (was_called[i] != 0) {
            printf("my_idx=%d, was_called[%d]=%d but should be 0\n", my_idx, i, was_called[i]);
            foundError++;
        }
    }
    for (i = my_idx; i < NUM_TEST_ATTRS; ++i) {
        if (was_called[i] != 1) {
            printf("my_idx=%d, was_called[%d]=%d but should be 1\n", my_idx, i, was_called[i]);
            foundError++;
        }
    }

    return MPI_SUCCESS;
}

}