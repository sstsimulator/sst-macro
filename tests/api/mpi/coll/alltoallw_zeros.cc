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

#include "mpitest.h"

namespace alltoallw_zeros {

int alltoallw_zeros(int argc, char *argv[])
{
    int sendbuf, recvbuf;
    int *sendcounts;
    int *recvcounts;
    int *sdispls;
    int *rdispls;
    MPI_Datatype sendtype;
    MPI_Datatype *sendtypes;
    MPI_Datatype *recvtypes;
    int rank = -1;
    int size = -1;
    int i;


    MPI_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    sendtypes = (MPI_Datatype*)malloc(size * sizeof(MPI_Datatype));
    recvtypes = (MPI_Datatype*)malloc(size * sizeof(MPI_Datatype));
    sendcounts = (int*)malloc(size * sizeof(int));
    recvcounts = (int*)malloc(size * sizeof(int));
    sdispls = (int*)malloc(size * sizeof(int));
    rdispls = (int*)malloc(size * sizeof(int));
    if (!sendtypes  || !recvtypes ||
        !sendcounts || !recvcounts ||
        !sdispls    || !rdispls)
    {
        printf("error, unable to allocate memory\n");
        goto fn_exit;
    }

    MPI_Type_contiguous(0, MPI_INT, &sendtype);
    MPI_Type_commit(&sendtype);

    for (i = 0; i < size; ++i) {
        sendtypes[i] = sendtype;
        sendcounts[i] = 1;
        sdispls[i] = 0;

        recvtypes[i] = MPI_INT;
        recvcounts[i] = 0;
        rdispls[i] = 0;
    }


    /** try zero-counts on both the send and recv side in case only one direction is broken for some reason */
    MPI_Alltoallw(&sendbuf, sendcounts, sdispls, sendtypes, &recvbuf, recvcounts, rdispls, recvtypes, MPI_COMM_WORLD);
    MPI_Alltoallw(&sendbuf, recvcounts, rdispls, recvtypes, &recvbuf, sendcounts, sdispls, sendtypes, MPI_COMM_WORLD);

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
    /** pass MPI_IN_PLACE and different but compatible types rank is even/odd */
    if (rank % 2)
        MPI_Alltoallw(MPI_IN_PLACE, NULL, NULL, NULL, &recvbuf, recvcounts, rdispls, recvtypes, MPI_COMM_WORLD);
    else
        MPI_Alltoallw(MPI_IN_PLACE, NULL, NULL, NULL, &recvbuf, sendcounts, sdispls, sendtypes, MPI_COMM_WORLD);
#endif

    /** now the same for Alltoallv instead of Alltoallw */
    MPI_Alltoallv(&sendbuf, sendcounts, sdispls, sendtypes[0], &recvbuf, recvcounts, rdispls, recvtypes[0], MPI_COMM_WORLD);
    MPI_Alltoallv(&sendbuf, recvcounts, rdispls, recvtypes[0], &recvbuf, sendcounts, sdispls, sendtypes[0], MPI_COMM_WORLD);

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
    if (rank % 2)
        MPI_Alltoallv(MPI_IN_PLACE, NULL, NULL, MPI_DATATYPE_NULL, &recvbuf, recvcounts, rdispls, recvtypes[0], MPI_COMM_WORLD);
    else
        MPI_Alltoallv(MPI_IN_PLACE, NULL, NULL, MPI_DATATYPE_NULL, &recvbuf, sendcounts, sdispls, sendtypes[0], MPI_COMM_WORLD);
#endif

    MPI_Type_free(&sendtype);

    if (rank == 0)
        printf(" No Errors\n");

fn_exit:
    if (rdispls)    free(rdispls);
    if (sdispls)    free(sdispls);
    if (recvcounts) free(recvcounts);
    if (sendcounts) free(sendcounts);
    if (recvtypes)  free(recvtypes);
    if (sendtypes)  free(sendtypes);

    MPI_Finalize();

    return 0;
}

}