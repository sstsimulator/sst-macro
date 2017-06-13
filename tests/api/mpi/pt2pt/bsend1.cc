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
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace bsend1 {
/** 
 * This is a simple program that tests bsend.  It may be run as a single
 * process to simplify debugging; in addition, bsend allows send-to-self
 * programs.
 */
int bsend1( int argc, char *argv[] )
{
    MPI_Comm comm = MPI_COMM_WORLD;
    int dest = 0, src = 0, tag = 1;
    int s1, s2, s3;
    char *buf, *bbuf;
    char msg1[7], msg3[17];
    double msg2[2];
    char rmsg1[64], rmsg3[64];
    double rmsg2[64];
    int errs = 0, rank;
    int bufsize, bsize;

    MTest_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    /** According to the standard, we must use the PACK_SIZE length of each
       message in the computation of the message buffer size */
    MPI_Pack_size( 7, MPI_CHAR, comm, &s1 );
    MPI_Pack_size( 2, MPI_DOUBLE, comm, &s2 );
    MPI_Pack_size( 17, MPI_CHAR, comm, &s3 );
    bufsize = 3 * MPI_BSEND_OVERHEAD + s1 + s2 + s3;
    buf = (char *)malloc( bufsize );
    MPI_Buffer_attach( buf, bufsize );

    strncpy( msg1, "012345", 7 );
    strncpy( msg3, "0123401234012341", 17 );
    msg2[0] = 1.23; msg2[1] = 3.21;

    if (rank == src) {
	/** These message sizes are chosen to expose any alignment problems */
	MPI_Bsend( msg1, 7, MPI_CHAR, dest, tag, comm );
	MPI_Bsend( msg2, 2, MPI_DOUBLE, dest, tag, comm );
	MPI_Bsend( msg3, 17, MPI_CHAR, dest, tag, comm );
    }

    if (rank == dest) {
	MPI_Recv( rmsg1, 7, MPI_CHAR, src, tag, comm, MPI_STATUS_IGNORE );
	MPI_Recv( rmsg2, 10, MPI_DOUBLE, src, tag, comm, MPI_STATUS_IGNORE );
	MPI_Recv( rmsg3, 17, MPI_CHAR, src, tag, comm, MPI_STATUS_IGNORE );

	if (strcmp( rmsg1, msg1 ) != 0) {
	    errs++;
	    fprintf( stderr, "message 1 (%s) should be %s\n", rmsg1, msg1 );
	}
	if (rmsg2[0] != msg2[0] || rmsg2[1] != msg2[1]) {
	    errs++;
	    fprintf( stderr, 
	  "message 2 incorrect, values are (%f,%f) but should be (%f,%f)\n",
		     rmsg2[0], rmsg2[1], msg2[0], msg2[1] );
	}
	if (strcmp( rmsg3, msg3 ) != 0) {
	    errs++;
	    fprintf( stderr, "message 3 (%s) should be %s\n", rmsg3, msg3 );
	}
    }

    /** We can't guarantee that messages arrive until the detach */
    MPI_Buffer_detach( &bbuf, &bsize );

    MTest_Finalize( errs );
    
    MPI_Finalize();
    return 0;
}

}