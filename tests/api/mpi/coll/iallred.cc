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
#include <assert.h>
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace iallred {
/** Since MPICH2 is currently the only NBC implementation in existence, just use
 * this quick-and-dirty #ifdef to decide whether to test the nonblocking
 * collectives.  Eventually we can add a configure option or configure test, or
 * the MPI-3 standard will be released and these can be gated on a MPI_VERSION
 * check */
#if !defined(USE_STRICT_MPI) && defined(MPICH2)
#define TEST_NBC_ROUTINES 1
#endif

int iallred(int argc, char *argv[])
{
    MPI_Request request;
    int size, rank;
    int one = 1, two = 2, isum, sum;
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    assert(size == 2);
#if defined(TEST_NBC_ROUTINES)
    MPIX_Iallreduce(&one,&isum,1,MPI_INT,MPI_SUM,MPI_COMM_WORLD,&request);
    MPI_Allreduce(&two,&sum,1,MPI_INT,MPI_SUM,MPI_COMM_WORLD);
    MPI_Wait(&request,MPI_STATUS_IGNORE);

    assert(isum == 2);
    assert(sum == 4);
    if (rank == 0)
        printf(" No errors\n");
#endif

    MPI_Finalize();
    return 0;
}

}