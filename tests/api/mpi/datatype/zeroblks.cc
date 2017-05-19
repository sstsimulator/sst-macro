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

namespace zeroblks {

int zeroblks( int argc, char *argv[] )
{
    int errs = 0;
    int position, pack_size, i;
    int dis[2], blklens[2];
    MPI_Datatype type;
    int send_buffer[60];
    int recv_buffer[60];
    int pack_buffer[1000];

    MTest_Init( &argc, &argv );

    /** Initialize data in the buffers */
    for (i=0; i<60; i++) {
	send_buffer[i] = i;
	recv_buffer[i] = -1;
	pack_buffer[i] = -2;
    }

    /** Create an indexed type with an empty first block */
    dis[0] = 0;
    dis[1] = 20;

    blklens[0] = 0;
    blklens[1] = 40;

    MPI_Type_indexed(2, blklens, dis, MPI_INT, &type);
    MPI_Type_commit(&type);

    position = 0;
    MPI_Pack( send_buffer, 1, type, pack_buffer, sizeof(pack_buffer), 
	      &position, MPI_COMM_WORLD );
    pack_size = position;
    position = 0;
    MPI_Unpack( pack_buffer, pack_size, &position, recv_buffer, 1, type, 
		MPI_COMM_WORLD );

    /** Check that the last 40 entries of the recv_buffer have the corresponding
       elements from the send buffer */
    for (i=0; i<20; i++) {
	if (recv_buffer[i] != -1) {
	    errs++;
	    fprintf( stderr, "recv_buffer[%d] = %d, should = -1\n", i, 
		     recv_buffer[i] );
	}
    }
    for (i=20; i<60; i++) {
	if (recv_buffer[i] != i) {
	    errs++;
	    fprintf( stderr, "recv_buffer[%d] = %d, should = %d\n", i, 
		     recv_buffer[i], i );
	}
    }
    MPI_Type_free( &type );

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;

}

}