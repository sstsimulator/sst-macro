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
#include "mpitest.h"
#include <stdio.h>
#include <stdlib.h>

namespace red_scat_block {
int red_scat_block(int argc, char **argv)
{
    int err = 0;
    int toterr, size, rank, i, sumval;
    int *sendbuf;
    int *recvbuf;
    MPI_Comm comm;

    MPI_Init(&argc, &argv);
    comm = MPI_COMM_WORLD;

    MPI_Comm_size(comm, &size);
    MPI_Comm_rank(comm, &rank);

    /** MPI_Reduce_scatter block was added in MPI-2.2 */
    sendbuf = (int *) malloc(size * sizeof(int));
    recvbuf = (int *) malloc(size * sizeof(int));
    if (!sendbuf || !recvbuf) {
        err++;
        fprintf(stderr, "unable to allocate send/recv buffers, aborting");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    for (i=0; i<size; i++)
        sendbuf[i] = rank + i;

    MPI_Reduce_scatter_block(sendbuf, recvbuf, 1, MPI_INT, MPI_SUM, comm);

    sumval = size * rank + ((size - 1) * size)/2;
    if (recvbuf[0] != sumval) {
        err++;
        fprintf(stdout, "Did not get expected value for reduce scatter block\n");
        fprintf(stdout, "[%d] Got %d expected %d\n", rank, recvbuf[0], sumval);
    }

    free(sendbuf);

    /** let's try it again with MPI_IN_PLACE this time */
    for (i=0; i<size; i++)
        recvbuf[i] = rank + i;

    MPI_Reduce_scatter_block(MPI_IN_PLACE, recvbuf, 1, MPI_INT, MPI_SUM, comm);

    sumval = size * rank + ((size - 1) * size)/2;
    if (recvbuf[0] != sumval) {
        err++;
        fprintf(stdout, "Did not get expected value for reduce scatter block\n");
        fprintf(stdout, "[%d] Got %d expected %d\n", rank, recvbuf[0], sumval);
    }
    free(recvbuf);

    MPI_Allreduce(&err, &toterr, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (rank == 0 && toterr == 0) {
        printf(" No Errors\n");
    }
    MPI_Finalize();

    return toterr;
}

}