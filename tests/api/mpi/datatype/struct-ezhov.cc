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
#include <string.h>

namespace struct_ezhov
{
#define COUNT		14
#define SIZE		340
#define EL_COUNT	1131

  global_arr<char, EL_COUNT * SIZE> s_buf;
  global_arr<char, EL_COUNT * SIZE> r_buf;

  int
  struct_ezhov(int argc, char **argv)
  {
    int rank, size, ret;
    MPI_Status Status;
    MPI_Request request;
    MPI_Datatype struct_type, type1[COUNT];
    MPI_Aint disp1[COUNT] =
    { 0, 0, 332, 340 };
    int block1[COUNT] =
    { 1, 56, 2, 1 };

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (rank == 0)
    {
      type1[0] = MPI_LB;
      type1[1] = MPI_FLOAT;
      type1[2] = MPI_FLOAT;
      type1[3] = MPI_UB;

      MPI_Type_struct(4, block1, disp1, type1, &struct_type);

      ret = MPI_Type_commit(&struct_type);
      if (ret != MPI_SUCCESS)
      {
        fprintf(stderr, "Could not make struct type."), fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, 1);
      }

      memset(s_buf, 0, EL_COUNT * SIZE);
      memset(r_buf, 0, EL_COUNT * SIZE);

      MPI_Isend(s_buf, EL_COUNT, struct_type, 0, 4, MPI_COMM_WORLD, &request);
      MPI_Recv(r_buf, EL_COUNT, struct_type, 0, 4, MPI_COMM_WORLD, &Status);
      MPI_Wait(&request, &Status);

      MPI_Type_free(&struct_type);
    }

    MPI_Finalize();

    printf(" No Errors\n");

    return 0;
  }

}