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
#include <string.h>
#include <sstmac/replacements/mpi.h>

namespace subarray {
#define X 64
#define Y 8
#define Z 512

double array[X][Y][Z];

int subarray(int argc, char *argv[])
{
    int myrank;
    MPI_Datatype subarray;
    int array_size[] = {X, Y, Z};
    int array_subsize[] = {X/2, Y/2, Z};
    int array_start[] = {0, 0, 0};
    int i, j, k;
    int errs = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    for (i = 0; i < X; ++i) {
        for (j = 0; j < Y; ++j) {
            for (k = 0; k < Z; ++k) {
                if (myrank == 0)
                    array[i][j][k] = 2.0;
                else
                    array[i][j][k] = -2.0;
            }
        }
    }

    MPI_Type_create_subarray(3, array_size, array_subsize, array_start, MPI_ORDER_C,
                             MPI_DOUBLE, &subarray);
    MPI_Type_commit(&subarray);

    if(myrank == 0)
        MPI_Send(array, 1, subarray, 1, 0, MPI_COMM_WORLD);
    else {
        MPI_Recv(array, 1, subarray, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        for (i = array_start[0]; i < array_subsize[0]; ++i) {
            for (j = array_start[1]; j < array_subsize[1]; ++j) {
                for (k = array_start[2]; k < array_subsize[2]; ++k) {
                    if (array[i][j][k] != 2.0)
                        ++errs;
                }
            }
        }
    }

    MPI_Type_free(&subarray);

    MPI_Allreduce(MPI_IN_PLACE, &errs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (myrank == 0) {
        if (errs)
            printf("Found %d errors\n", errs);
        else
            printf(" No Errors\n");
    }
    MPI_Finalize();

    return 0;
}

}