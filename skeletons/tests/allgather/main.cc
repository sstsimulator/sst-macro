/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <mpi.h>
#include <stddef.h>
#include <iostream>

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (rank == 0){
    std::cout << "Starting collective" << std::endl;
  }

  int nelems = 100;
//#define VALIDATE_BUFFERS
#ifdef VALIDATE_BUFFERS
  int* send_buf = new int[nelems];
  int* recv_buf = new int[nelems * size];
  for (int i=0; i < nelems; ++i){
    send_buf[i] = rank;
  }
  for (int i=0; i < size; ++i){
    for (int j=0; j < nelems; ++j){
      recv_buf[i*nelems + j] = -1;
    }
  }
#else
  void* send_buf = sstmac_nullptr_send;
  void* recv_buf = sstmac_nullptr_recv;
#endif

  MPI_Allgather(send_buf, 100, MPI_INT,
                recv_buf, 100, MPI_INT, MPI_COMM_WORLD);

  if (rank == 0){
    std::cout << "Cleared collective" << std::endl;
  }
#ifdef VALIDATE_BUFFERS
  for (int i=0; i < size; ++i){
    int* values = recv_buf + i*nelems;
    for (int j=0; j < nelems; ++j){
      if (values[j] != i){
        printf("V[%d][%d] = %d != %d\n", i, j, values[j], i);
      }
    }
  }
#endif

  MPI_Finalize();

  if (rank == 0){
    std::cout << "Cleared finalize" << std::endl;
  }

  return 0;
}
