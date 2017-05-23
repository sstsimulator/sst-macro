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
#include <stddef.h>
#include <assert.h>

namespace get_elements {
/** Tests MPI_Get_elements with a contiguous datatype that triggered a bug in
 * past versions of MPICH2.  See ticket #1467 for more info. */

struct test_struct {
    char a;
    short b;
    int c;
};

int get_elements(int argc, char **argv)
{
    int rank, count;
    struct test_struct sendbuf, recvbuf;
    int blens[3];
    MPI_Aint displs[3];
    MPI_Datatype types[3];
    MPI_Datatype struct_type, contig;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /** We use a contig of a struct in order to satisfy two properties:
     * (A) a type that contains more than one element type (the struct portion)
     * (B) a type that has an odd number of ints in its "type contents" (1 in
     *     this case)
     * This triggers a specific bug in some versions of MPICH2. */
    blens[0]  = 1;
    displs[0] = offsetof(struct test_struct, a);
    types[0]  = MPI_CHAR;
    blens[1]  = 1;
    displs[1] = offsetof(struct test_struct, b);
    types[1]  = MPI_SHORT;
    blens[2]  = 1;
    displs[2] = offsetof(struct test_struct, c);
    types[2]  = MPI_INT;
    MPI_Type_create_struct(3, blens, displs, types, &struct_type);
    MPI_Type_contiguous(1, struct_type, &contig);
    MPI_Type_commit(&struct_type);
    MPI_Type_commit(&contig);

    sendbuf.a = 20;
    sendbuf.b = 30;
    sendbuf.c = 40;
    recvbuf.a = -1;
    recvbuf.b = -1;
    recvbuf.c = -1;

    /** send to ourself */
    MPI_Sendrecv(&sendbuf, 1, contig, 0, 0,
                 &recvbuf, 1, contig, 0, 0,
                 MPI_COMM_SELF, &status);

    /** sanity */
    assert(sendbuf.a == recvbuf.a);
    assert(sendbuf.b == recvbuf.b);
    assert(sendbuf.c == recvbuf.c);

    /** now check that MPI_Get_elements returns the correct answer and that the
     * library doesn't explode in the process */
    count = 0xdeadbeef;
    MPI_Get_elements(&status, contig, &count);
    MPI_Type_free(&struct_type);
    MPI_Type_free(&contig);

    if (count != 3) {
        printf("unexpected value for count, expected 3, got %d\n", count);
    }
    else {
        if (rank == 0) {
            printf(" No Errors\n");
        }
    }

    MPI_Finalize();
    return 0;
}

}