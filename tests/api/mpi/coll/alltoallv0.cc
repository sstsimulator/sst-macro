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
#include <stdlib.h>
#include <stdio.h>

namespace alltoallv0 {

/**
  This program tests MPI_Alltoallv by having processor each process 
  send data to two neighbors only, using counts of 0 for the other processes.
  This idiom is sometimes used for halo exchange operations.

  Because there are separate send and receive types to alltoallv,
  there need to be tests to rearrange data on the fly.  Not done yet.
  
  Currently, the test uses only MPI_INT; this is adequate for testing systems
  that use point-to-point operations
 */

int alltoallv0( int argc, char **argv )
{

    MPI_Comm comm;
    int      *sbuf, *rbuf;
    int      rank, size;
    int      *sendcounts, *recvcounts, *rdispls, *sdispls;
    int      i, *p, err;
    int      left, right, length;
    
    MTest_Init( &argc, &argv );
    err = 0;
    
    while (MTestGetIntracommGeneral( &comm, 2, 1 )) {
      if (comm == MPI_COMM_NULL) continue;

      MPI_Comm_size( comm, &size );
      MPI_Comm_rank( comm, &rank );
      
      if (size < 3) continue;

      /** Create and load the arguments to alltoallv */
      sendcounts = (int *)malloc( size * sizeof(int) );
      recvcounts = (int *)malloc( size * sizeof(int) );
      rdispls    = (int *)malloc( size * sizeof(int) );
      sdispls    = (int *)malloc( size * sizeof(int) );
      if (!sendcounts || !recvcounts || !rdispls || !sdispls) {
	fprintf( stderr, "Could not allocate arg items!\n" );
	MPI_Abort( comm, 1 );
      }

      /** Get the neighbors */
      left  = (rank - 1 + size) % size;
      right = (rank + 1) % size;

      /** Set the defaults */
      for (i=0; i<size; i++) {
	  sendcounts[i] = 0;
	  recvcounts[i] = 0;
	  rdispls[i]    = 0;
	  sdispls[i]    = 0;
      }

      for (length=1; length < 66000; length = length*2+1 ) {
	  /** Get the buffers */
	  sbuf = (int *)malloc( 2 * length * sizeof(int) );
	  rbuf = (int *)malloc( 2 * length * sizeof(int) );
	  if (!sbuf || !rbuf) {
	      fprintf( stderr, "Could not allocate buffers!\n" );
	      MPI_Abort( comm, 1 );
	  }
	  
	  /** Load up the buffers */
	  for (i=0; i<length; i++) {
	      sbuf[i]        = i + 100000*rank;
	      sbuf[i+length] = i + 100000*rank;
	      rbuf[i]        = -i;
	      rbuf[i+length] = -i-length;
	  }
	  sendcounts[left]  = length;
	  sendcounts[right] = length;
	  recvcounts[left]  = length;
	  recvcounts[right] = length;
	  rdispls[left]     = 0;
	  rdispls[right]    = length;
	  sdispls[left]     = 0;
	  sdispls[right]    = length;
      
	  MPI_Alltoallv( sbuf, sendcounts, sdispls, MPI_INT,
			 rbuf, recvcounts, rdispls, MPI_INT, comm );
      
	  /** Check rbuf */
	  p = rbuf;          /** left */

	  for (i=0; i<length; i++) {
	      if (p[i] != i + 100000 * left) {
		  if (err < 10) {
		      fprintf( stderr, "[%d from %d] got %d expected %d for %dth\n", 
			       rank, left, p[i], i + 100000 * left, i );
		  }
		  err++;
	      }
	  }

	  p = rbuf + length; /** right */
	  for (i=0; i<length; i++) {
	      if (p[i] != i + 100000 * right) {
		  if (err < 10) {
		      fprintf( stderr, "[%d from %d] got %d expected %d for %dth\n", 
			       rank, right, p[i], i + 100000 * right, i );
		  }
		  err++;
	      }
	  }

	  free( rbuf );
	  free( sbuf );
      }
	  
      free( sdispls );
      free( rdispls );
      free( recvcounts );
      free( sendcounts );
      MTestFreeComm( &comm );
    }

    MTest_Finalize( err );
    MPI_Finalize();
    return 0;
}

}