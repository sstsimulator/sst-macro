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

namespace bcast3 {

/**
static char MTEST_Descrip[] = "Test of broadcast with various roots and datatypes and sizes that are not powers of two";
*/

int bcast3( int argc, char *argv[] )
{
    int errs = 0, err;
    int rank, size, root;
    int minsize = 2, count; 
    MPI_Comm      comm;
    MTestDatatype sendtype, recvtype;

    MTest_Init( &argc, &argv );

    /** The following illustrates the use of the routines to 
       run through a selection of communicators and datatypes.
       Use subsets of these for tests that do not involve combinations 
       of communicators, datatypes, and counts of datatypes */
    while (MTestGetIntracommGeneral( &comm, minsize, 1 )) {
	if (comm == MPI_COMM_NULL) continue;
	/** Determine the sender and receiver */
	MPI_Comm_rank( comm, &rank );
	MPI_Comm_size( comm, &size );
	
	count = 1;
	/** This must be very large to ensure that we reach the long message
	   algorithms */
	for (count = 4; count < 66000; count = count * 4) {
	    while (MTestGetDatatypes( &sendtype, &recvtype, count-1 )) {
		for (root=0; root<size; root++) {
		    if (rank == root) {
			sendtype.InitBuf( &sendtype );
			err = MPI_Bcast( sendtype.buf, sendtype.count,
					 sendtype.datatype, root, comm );
			if (err) {
			    errs++;
			    MTestPrintError( err );
			}
		    }
		    else {
			recvtype.InitBuf( &recvtype );
			err = MPI_Bcast( recvtype.buf, recvtype.count, 
				    recvtype.datatype, root, comm );
			if (err) {
			    errs++;
			    fprintf( stderr, "Error with communicator %s and datatype %s\n", 
				 MTestGetIntracommName(), 
				 MTestGetDatatypeName( &recvtype ) );
			    MTestPrintError( err );
			}
			err = MTestCheckRecv( 0, &recvtype );
			if (err) {
			    errs += errs;
			}
		    }
		}
		MTestFreeDatatype( &recvtype );
		MTestFreeDatatype( &sendtype );
	    }
	}
	MTestFreeComm( &comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}