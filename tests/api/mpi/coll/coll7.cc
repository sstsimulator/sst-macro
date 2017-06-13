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
#include "mpitest.h"

namespace coll7 {
#define MAX_PROCESSES 10

int coll7( int argc, char **argv )
{
    int              rank, size, i,j;
    int              table[MAX_PROCESSES][MAX_PROCESSES];
    int              errors=0;
    int              participants;

    MTest_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );

    /** A maximum of MAX_PROCESSES processes can participate */
    if ( size > MAX_PROCESSES ) participants = MAX_PROCESSES;
    else              participants = size;
    if (MAX_PROCESSES % participants) {
	fprintf( stderr, "Number of processors must divide %d\n",
		MAX_PROCESSES );
	MPI_Abort( MPI_COMM_WORLD, 1 );
	}
    /** while (MAX_PROCESSES % participants) participants--; */
    if ( (rank < participants) ) {

      /** Determine what rows are my responsibility */
      int block_size = MAX_PROCESSES / participants;
      int begin_row  = rank * block_size;
      int end_row    = (rank+1) * block_size;
      int send_count = block_size * MAX_PROCESSES;
      int recv_count = send_count;

      /** Paint my rows my color */
      for (i=begin_row; i<end_row ;i++)
	for (j=0; j<MAX_PROCESSES; j++)
	  table[i][j] = rank + 10;

      /** Everybody gets the gathered table */
      MPI_Allgather(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL,
                    &table[0][0], recv_count, MPI_INT, MPI_COMM_WORLD);

      /** Everybody should have the same table now,  */
      /** This test does not in any way guarantee there are no errors */
      /** Print out a table or devise a smart test to make sure it's correct */
      for (i=0; i<MAX_PROCESSES;i++) {
	if ( (table[i][0] - table[i][MAX_PROCESSES-1] !=0) ) 
	  errors++;
      }
    } 

    MTest_Finalize( errors );
    MPI_Finalize();
    return MTestReturnValue( errors );
}

}