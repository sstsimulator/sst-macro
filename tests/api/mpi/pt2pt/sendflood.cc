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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sstmac/replacements/mpi.h>

namespace sendflood {
/**
 * Run this test with 8 processes.  This test was submitted by xxx
 * as a result of problems seen with the ch3:shm device on a Solaris 
 * system.  The symptom is that the test hangs; this is due to losing 
 * a message, probably due to a race condition in a message-queue update.
 * As a test for race conditions, it may need to be run multiple times
 * to expose a problem if a problem does exist.
 */

#define LOOP_COUNT  100
#define DATA_SIZE   4
#define MP_TAG      999

#define PROGRESS_COUNT 0xfff
static int verbose = 0;
static int loopProgress = 0;

int sendflood( int argc, char *argv[] )
{
    int     nProc, rank ;
    int     i, j, status ;
    FILE    *pf=0 ;

    MPI_Init( &argc, &argv ) ;
    MPI_Comm_size( MPI_COMM_WORLD, &nProc ) ;
    MPI_Comm_rank( MPI_COMM_WORLD, &rank ) ;

    for (i=1; i<argc; i++) {
	if (strcmp(argv[i],"-v") == 0 ||
	    strcmp(argv[i],"--verbose") == 0) verbose = 1;
	else if (strcmp(argv[i],"-p") == 0 ||
		 strcmp(argv[i],"--progress") == 0) loopProgress = 1;
	else {
	    if (rank == 0) {
		fprintf( stderr, "%s: [ -v | --verbose ] [ -p | --progress ]\n",
			 argv[0] );
		fflush(stderr);
	    }
	}
    }

    if (verbose) {
	char    buf[ 128 ] ;
	sprintf( buf, "fast_mpi_%d.dmp", rank ) ;
	pf = fopen( buf, "w" ) ;
    }
    else if (loopProgress) {
	pf = stdout;
    }

    if( !rank ) {
       int      **psend ;
       int      **precv ;
       psend = (int**)calloc( nProc, sizeof( int *) ) ;
       precv = (int**)calloc( nProc, sizeof( int *) ) ;
       for( i = 0 ; i < nProc ; i++ ) {
           psend[ i ] = (int*)calloc( DATA_SIZE, sizeof( int ) ) ;
           precv[ i ] = (int*)calloc( DATA_SIZE, sizeof( int ) ) ;
       }
       for( i = 0 ; i < LOOP_COUNT ; i++ ) {
	   if (verbose) {
	       fprintf( pf, "Master : loop %d\n", i ) ;
	       fflush( pf ) ;
	   }
	   else if (loopProgress && (i & PROGRESS_COUNT) == 0) {
	     fprintf( pf, "Master: loop %d\n", i ); fflush( pf );
	   }
          for( j = 1 ; j < nProc ; j++ ) {
	      if (verbose) {
		  fprintf( pf, "  read from child %d\n", j ) ;
		  fflush( pf ) ;
	      }
             status = MPI_Recv( precv[ j ], DATA_SIZE, MPI_INT, j, MP_TAG,
				MPI_COMM_WORLD, MPI_STATUS_IGNORE ) ;
	     if (verbose) {
		 fprintf( pf, "  read from child %d done, status = %d\n", j,
			  status ) ;
		 fflush( pf ) ;
	     }
          }
          for( j = 1 ; j < nProc ; j++ ) {
	      if (verbose) {
		  fprintf( pf, "  send to child %d\n", j ) ;
		  fflush( pf ) ;
	      }
             status = MPI_Send( psend[ j ], DATA_SIZE - 1, MPI_INT, j,
				MP_TAG, MPI_COMM_WORLD ) ;
	     if (verbose) {
		 fprintf( pf, "  send to child %d done, status = %d\n", j,
			  status ) ;
		 fflush( pf ) ;
	     }
          }
       }
       for( i = 0 ; i < nProc ; i++ ) {
	   free( psend[ i ] );
	   free( precv[ i ] );
       }
       free( psend );
       free( precv );
    } else {
       int  *psend ;
       int  *precv ;
       psend = (int*)calloc( DATA_SIZE, sizeof( int ) ) ;
       precv = (int*)calloc( DATA_SIZE, sizeof( int ) ) ;
       for( i = 0 ; i < LOOP_COUNT ; i++ ) {
	   if (verbose) {
	       fprintf( pf, "  send to master\n" ) ;
	       fflush( pf ) ;
	   }
	   /**
	   else if (loopProgress && (i & PROGRESS_COUNT) == 0) {
	     fprintf( pf, "Slave: loop %d\n", i ); fflush( pf );
	   }
	   */
	   status = MPI_Send( psend, DATA_SIZE - 1, MPI_INT, 0, MP_TAG,
			      MPI_COMM_WORLD ) ;
	   if (verbose) {
	       fprintf( pf, "  send to master done, status = %d\n", status ) ;
	       fflush( pf ) ;
	       fprintf( pf, "  read from master\n" ) ;
	       fflush( pf ) ;
	   }
	   status = MPI_Recv( precv, DATA_SIZE, MPI_INT, 0, MP_TAG,
			      MPI_COMM_WORLD, MPI_STATUS_IGNORE ) ;
	   if (verbose) {
	       fprintf( pf, "  read from master done, status = %d\n", status ) ;
	       fflush( pf ) ;
	   }
       }
       free( psend );
       free( precv );
    }
    if (verbose) {
	fclose( pf ) ;
    }
    MPI_Finalize() ;

    /** This test fails if it hangs */
    if (rank == 0) {
	printf( " No Errors\n" );
    }

    return 0;
}

}