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
#include <stdlib.h>
#include "mpitest.h"

namespace large_message {
/** tests send/recv of a message > 2GB. count=270M, type=long long 
   run with 3 processes to exercise both shared memory and TCP in Nemesis tests*/

int large_message(int argc, char *argv[]) 
{
  int        ierr,i,size,rank;
  int        cnt = 270000000;
  MPI_Status status;
  long long  *cols;
  int errs = 0;


  MTest_Init(&argc,&argv); 

/** need large memory */
  if (sizeof(void *) < 8) {
      MTest_Finalize(errs);
      MPI_Finalize();
      return 0;
  }

  ierr = MPI_Comm_size(MPI_COMM_WORLD,&size);
  ierr = MPI_Comm_rank(MPI_COMM_WORLD,&rank);
 // if (size != 3) {
  //  fprintf(stderr,"[%d] usage: mpiexec -n 3 %s\n",rank,argv[0]);
  //  MPI_Abort(MPI_COMM_WORLD,1);
  //}

  if(rank < 3){

  cols = (long long*)malloc(cnt*sizeof(long long));
  if (cols == NULL) {
      printf("malloc of >2GB array failed\n");
      errs++;
      MTest_Finalize(errs);
      MPI_Finalize();
      return 0;
  }

  if (rank == 0) {
    for (i=0; i<cnt; i++) cols[i] = i;
    /** printf("[%d] sending...\n",rank);*/
    ierr = MPI_Send(cols,cnt,MPI_LONG_LONG_INT,1,0,MPI_COMM_WORLD);
    ierr = MPI_Send(cols,cnt,MPI_LONG_LONG_INT,2,0,MPI_COMM_WORLD);
  } else {
      /** printf("[%d] receiving...\n",rank); */
    for (i=0; i<cnt; i++) cols[i] = -1;
    ierr = MPI_Recv(cols,cnt,MPI_LONG_LONG_INT,0,0,MPI_COMM_WORLD,&status);
    /** ierr = MPI_Get_count(&status,MPI_LONG_LONG_INT,&cnt);
       Get_count still fails because status.count is not 64 bit */
    for (i=0; i<cnt; i++) {
        if (cols[i] != i) {
            /**printf("Rank %d, cols[i]=%lld, should be %d\n", rank, cols[i], i);*/
            errs++;
        }
    }
  }
  }
  MTest_Finalize(errs);
  MPI_Finalize();
  return 0;
}

}