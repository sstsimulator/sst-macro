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

namespace getpartelm {
/**
static char MTest_descrip[] = "Receive partial datatypes and check that\
MPI_Getelements gives the correct version";
*/

int getpartelm( int argc, char *argv[] )
{
    int errs = 0;
    MPI_Datatype outtype, oldtypes[2];
    MPI_Aint     offsets[2];
    int          blklens[2];
    MPI_Comm     comm;
    int          size, rank, src, dest, tag;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );
    
    if (size < 2) {
	errs++;
	printf( "This test requires at least 2 processes\n" );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    
    src  = 0;
    dest = 1;

    if (rank == src) {
	int buf[128], position, cnt;
	/** sender */

	/** Create a datatype and send it (multiple of sizeof(int)) */
	/** Create a send struct type */
	oldtypes[0] = MPI_INT;
	oldtypes[1] = MPI_CHAR;
	blklens[0]  = 1;
	blklens[1]  = 4*sizeof(int);
	offsets[0]  = 0;
	offsets[1]  = sizeof(int);
	MPI_Type_struct( 2, blklens, offsets, oldtypes, &outtype );
	MPI_Type_commit( &outtype );

	buf[0] = 4*sizeof(int);
	/** printf( "About to send to %d\n", dest ); */
	MPI_Send( buf, 1, outtype, dest, 0, comm );
	MPI_Type_free( &outtype );

	/** Create a datatype and send it (not a multiple of sizeof(int)) */
	/** Create a send struct type */
	oldtypes[0] = MPI_INT;
	oldtypes[1] = MPI_CHAR;
	blklens[0]  = 1;
	blklens[1]  = 4*sizeof(int)+1;
	offsets[0]  = 0;
	offsets[1]  = sizeof(int);
	MPI_Type_struct( 2, blklens, offsets, oldtypes, &outtype );
	MPI_Type_commit( &outtype );

	buf[0] = 4*sizeof(int) + 1;
	MPI_Send( buf, 1, outtype, dest, 1, comm );
	MPI_Type_free( &outtype );

	/** Pack data and send as packed */
	position = 0;
	cnt = 7;
	MPI_Pack( &cnt, 1, MPI_INT, 
		  buf, 128*sizeof(int), &position, comm );
	MPI_Pack( (void*)"message", 7, MPI_CHAR,
		  buf, 128*sizeof(int), &position, comm );
	MPI_Send( buf, position, MPI_PACKED, dest, 2, comm );
    }
    else if (rank == dest) {
	MPI_Status status;
	int        buf[128], i, elms, count;

	/** Receiver */
	/** Create a receive struct type */
	oldtypes[0] = MPI_INT;
	oldtypes[1] = MPI_CHAR;
	blklens[0]  = 1;
	blklens[1]  = 256;
	offsets[0]  = 0;
	offsets[1]  = sizeof(int);
	MPI_Type_struct( 2, blklens, offsets, oldtypes, &outtype );
	MPI_Type_commit( &outtype );

	for (i=0; i<3; i++) {
	    tag = i;
	    /** printf( "about to receive tag %d from %d\n", i, src ); */
	    MPI_Recv( buf, 1, outtype, src, tag, comm, &status );
	    MPI_Get_elements( &status, outtype, &elms );
	    if (elms != buf[0] + 1) {
		errs++;
		printf( "For test %d, Get elements gave %d but should be %d\n",
			i, elms, buf[0] + 1 );
	    }
	    MPI_Get_count( &status, outtype, &count );
	    if (count != MPI_UNDEFINED) {
		errs++;
		printf( "For partial send, Get_count did not return MPI_UNDEFINED\n" );
	    }
	}
	MPI_Type_free( &outtype );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}