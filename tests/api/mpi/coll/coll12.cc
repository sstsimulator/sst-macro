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

namespace coll12 {
#define TABLE_SIZE 2

int coll12( int argc, char **argv )
{
  int    rank, size;
  double a[TABLE_SIZE];
  struct { double a; int b; } in[TABLE_SIZE], out[TABLE_SIZE];
  int    i;
  int    errors = 0, toterrors;

  /** Initialize the environment and some variables */
  MTest_Init( &argc, &argv );
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );
  MPI_Comm_size( MPI_COMM_WORLD, &size );

  /** Initialize the maxloc data */
  for ( i=0; i<TABLE_SIZE; i++ ) a[i] = 0;
  for ( i=rank; i<TABLE_SIZE; i++ ) a[i] = (double)rank + 1.0;

  /** Copy data to the "in" buffer */
  for (i=0; i<TABLE_SIZE; i++) { 
	in[i].a = a[i];
	in[i].b = rank;
  }

  /** Reduce it! */
  MPI_Reduce( in, out, TABLE_SIZE, MPI_DOUBLE_INT, MPI_MAXLOC, 0, MPI_COMM_WORLD );
  MPI_Bcast ( out, TABLE_SIZE, MPI_DOUBLE_INT, 0, MPI_COMM_WORLD );

  /** Check to see that we got the right answers */
  for (i=0; i<TABLE_SIZE; i++) 
	if (i % size == rank)
	  if (out[i].b != rank) {
        printf("MAX (ranks[%d] = %d != %d\n", i, out[i].b, rank );
		errors++;
      }

  /** Initialize the minloc data */
  for ( i=0; i<TABLE_SIZE; i++ ) a[i] = 0;
  for ( i=rank; i<TABLE_SIZE; i++ ) a[i] = -(double)rank - 1.0;

  /** Copy data to the "in" buffer */
  for (i=0; i<TABLE_SIZE; i++)  {
	in[i].a = a[i];
	in[i].b = rank;
  }

  /** Reduce it! */
  MPI_Allreduce( in, out, TABLE_SIZE, MPI_DOUBLE_INT, MPI_MINLOC, MPI_COMM_WORLD );

  /** Check to see that we got the right answers */
  for (i=0; i<TABLE_SIZE; i++) 
	if (i % size == rank)
	  if (out[i].b != rank) {
        printf("MIN (ranks[%d] = %d != %d\n", i, out[i].b, rank );
		errors++;
      }

  /** Finish up! */
  MTest_Finalize( errors );
  MPI_Finalize();
  return MTestReturnValue( errors );
}

}