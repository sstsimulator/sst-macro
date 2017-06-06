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
#include <string.h>
#include "mpitest.h"

namespace ctxsplit {

/**
 * This check is intended to fail if there is a leak of context ids.  
 * Because this is trying to exhaust the number of context ids, it needs
 * to run for a longer time than many tests.  The for loop uses 100,000 
 * iterations, which is adequate for MPICH2 (with only about 1k context ids
 * available).
 */

int ctxsplit(int argc, char** argv) {

   int      i=0;
   int      randval;
   int      rank;
   int      errs = 0;
   MPI_Comm newcomm;
   double   startTime;
   int      nLoop = 100000;
   
   MTest_Init(&argc,&argv);

   for (i=1; i<argc; i++) {
       if (strcmp( argv[i], "--loopcount" ) == 0)  {
	   i++;
	   nLoop = atoi( argv[i] );
       }
       else {
	   fprintf( stderr, "Unrecognized argument %s\n", argv[i] );
       }
   }

   MPI_Comm_rank(MPI_COMM_WORLD,&rank);

   startTime = MPI_Wtime();
   for (i=0; i<nLoop; i++) {
       
       if ( rank == 0 && (i%100 == 0) ) {
	   double rate = MPI_Wtime() - startTime;
	   if (rate > 0) {
	       rate = i / rate;
	       MTestPrintfMsg( 10, "After %d (%f)\n", i, rate );
	   }
	   else {
	       MTestPrintfMsg( 10, "After %d\n", i );
	   }
       }
       
       /** FIXME: Explain the rationale behind rand in this test */
       randval=rand();
       
       if (randval%(rank+2) == 0) {
	   MPI_Comm_split(MPI_COMM_WORLD,1,rank,&newcomm);
	   MPI_Comm_free( &newcomm );
       }
       else {
	   MPI_Comm_split(MPI_COMM_WORLD,MPI_UNDEFINED,rank,&newcomm);
	   if (newcomm != MPI_COMM_NULL) {
	       errs++;
	       printf( "Created a non-null communicator with MPI_UNDEFINED\n" );
	   }
       }
       
   }
   
   MTest_Finalize( errs );
   MPI_Finalize();
   
   return 0;
}

}