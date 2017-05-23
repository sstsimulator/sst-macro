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
#include <string.h>

namespace alltoallw2 {
/**
  This program tests MPI_Alltoallw by having processor i send different
  amounts of data to each processor.  This is just the MPI_Alltoallv test,
  but with displacements in bytes rather than units of the datatype.

  Because there are separate send and receive types to alltoallw,
  there need to be tests to rearrange data on the fly.  Not done yet.
  
  The first test sends i items to processor i from all processors.

  Currently, the test uses only MPI_INT; this is adequate for testing systems
  that use point-to-point operations
 */

int alltoallw2( int argc, char **argv )
{

    MPI_Comm comm;
    int      *sbuf, *rbuf;
    int      rank, size;
    int      *sendcounts, *recvcounts, *rdispls, *sdispls;
    int      i, j, *p, err;
    MPI_Datatype *sendtypes, *recvtypes;
    
    MTest_Init( &argc, &argv );
    err = 0;
    
    while (MTestGetIntracommGeneral( &comm, 2, 1 )) {
      if (comm == MPI_COMM_NULL) continue;

      /** Create the buffer */
      MPI_Comm_size( comm, &size );
      MPI_Comm_rank( comm, &rank );
      sbuf = (int *)malloc( size * size * sizeof(int) );
      rbuf = (int *)malloc( size * size * sizeof(int) );
      if (!sbuf || !rbuf) {
	fprintf( stderr, "Could not allocated buffers!\n" );
	MPI_Abort( comm, 1 );
      }
      
      /** Load up the buffers */
      for (i=0; i<size*size; i++) {
	sbuf[i] = i + 100*rank;
	rbuf[i] = -i;
      }
      
      /** Create and load the arguments to alltoallv */
      sendcounts = (int *)malloc( size * sizeof(int) );
      recvcounts = (int *)malloc( size * sizeof(int) );
      rdispls    = (int *)malloc( size * sizeof(int) );
      sdispls    = (int *)malloc( size * sizeof(int) );
      sendtypes    = (MPI_Datatype *)malloc( size * sizeof(MPI_Datatype) );
      recvtypes    = (MPI_Datatype *)malloc( size * sizeof(MPI_Datatype) );
      if (!sendcounts || !recvcounts || !rdispls || !sdispls || !sendtypes || !recvtypes) {
	fprintf( stderr, "Could not allocate arg items!\n" );
	MPI_Abort( comm, 1 );
      }
      /** Note that process 0 sends no data (sendcounts[0] = 0) */
      for (i=0; i<size; i++) {
	sendcounts[i] = i;
	recvcounts[i] = rank;
	rdispls[i]    = i * rank * sizeof(int);
	sdispls[i]    = (((i+1) * (i))/2) * sizeof(int);
        sendtypes[i] = recvtypes[i] = MPI_INT;
      }
      MPI_Alltoallw( sbuf, sendcounts, sdispls, sendtypes,
		     rbuf, recvcounts, rdispls, recvtypes, comm );
      
      /** Check rbuf */
      for (i=0; i<size; i++) {
	p = rbuf + rdispls[i]/sizeof(int);
	for (j=0; j<rank; j++) {
	  if (p[j] != i * 100 + (rank*(rank+1))/2 + j) {
	    fprintf( stderr, "[%d] got %d expected %d for %dth\n",
		     rank, p[j],(i*(i+1))/2 + j, j );
	    err++;
	  }
	}
      }

      free(sendtypes);
      free(sdispls);
      free(sendcounts);
      free(sbuf);

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
      /** check MPI_IN_PLACE, added in MPI-2.2 */
      free( rbuf );
      rbuf = (int *)malloc( size * (2 * size) * sizeof(int) );
      if (!rbuf) {
        fprintf( stderr, "Could not reallocate rbuf!\n" );
        MPI_Abort( comm, 1 );
      }

      /** Load up the buffers */
      for (i = 0; i < size; i++) {
        /** alltoallw displs are in bytes, not in type extents */
        rdispls[i]    = i * (2 * size) * sizeof(int);
        recvtypes[i]  = MPI_INT;
        recvcounts[i] = i + rank;
      }
      memset(rbuf, -1, size * (2 * size) * sizeof(int));
      for (i=0; i < size; i++) {
        p = rbuf + (rdispls[i] / sizeof(int));
        for (j = 0; j < recvcounts[i]; ++j) {
          p[j] = 100 * rank + 10 * i + j;
        }
      }

      MPI_Alltoallw( MPI_IN_PLACE, NULL, NULL, NULL,
                     rbuf, recvcounts, rdispls, recvtypes, comm );

      /** Check rbuf */
      for (i=0; i<size; i++) {
        p = rbuf + (rdispls[i] / sizeof(int));
        for (j=0; j<recvcounts[i]; j++) {
          int expected = 100 * i + 10 * rank + j;
          if (p[j] != expected) {
            fprintf(stderr, "[%d] got %d expected %d for block=%d, element=%dth\n",
                    rank, p[j], expected, i, j);
            ++err;
          }
        }
      }
#endif

      free(recvtypes);
      free(rdispls);
      free(recvcounts);
      free(rbuf);
      MTestFreeComm( &comm );
    }

    MTest_Finalize( err );
    MPI_Finalize();
    return 0;
}

}