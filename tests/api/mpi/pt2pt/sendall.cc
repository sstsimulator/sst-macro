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


namespace sendall {
/** 
 * This test makes sure that each process can send to each other process.
 * If there are bugs in the handling of request completions or in 
 * queue operations, then this test may fail on them (it did with
 * early EagerShort handling).
 */

#define MAXPES 32
#define MYBUFSIZE 16*1024
static int buffer[MAXPES][MYBUFSIZE];

#define NUM_RUNS 10

int sendall ( int argc, char *argv[] )
{
  int i;
  int count, size;
  int self, npes;
  double secs;
  MPI_Request request[MAXPES];
  MPI_Status status;

  MTest_Init (&argc, &argv);
  MPI_Comm_rank (MPI_COMM_WORLD, &self);
  MPI_Comm_size (MPI_COMM_WORLD, &npes);

  if (npes > MAXPES) {
    fprintf( stderr, "This program requires a comm_world no larger than %d",
	     MAXPES );
    MPI_Abort( MPI_COMM_WORLD, 1 );
  }

  for (size = 1; size  <= MYBUFSIZE ; size += size) {
      secs = -MPI_Wtime ();
      for (count = 0; count < NUM_RUNS; count++) {
	  MPI_Barrier (MPI_COMM_WORLD);

	  for (i = 0; i < npes; i++) {
	      if (i != self)
		MPI_Irecv (buffer[i], size, MPI_INT, i,
			 MPI_ANY_TAG, MPI_COMM_WORLD, &request[i]);
	    }

	  for (i = 0; i < npes; i++) {
	      if (i != self)
		MPI_Send (buffer[self], size, MPI_INT, i, 0, MPI_COMM_WORLD);
	    }

	  for (i = 0; i < npes; i++) {
	      if (i != self)
		MPI_Wait (&request[i], &status);
	    }

	}
      MPI_Barrier (MPI_COMM_WORLD);
      secs += MPI_Wtime ();

      if (self == 0) {
	  secs = secs / (double) NUM_RUNS;
	  MTestPrintfMsg( 1, "length = %d ints\n", size );
	}
    }

  /** Simple completion is all that we normally ask of this program */

  MTest_Finalize( 0 );

  MPI_Finalize();
  return 0;
}

}