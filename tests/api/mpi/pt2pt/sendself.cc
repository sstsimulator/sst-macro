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

namespace sendself {
/**
static char MTEST_Descrip[] = "Test of sending to self (with a preposted receive)";
*/

int sendself( int argc, char *argv[] )
{
    int errs = 0, err;
    int rank, size;
    int count;
    MPI_Comm      comm;
    MPI_Request   req;
    MTestDatatype sendtype, recvtype;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( comm, MPI_ERRORS_RETURN );
    
    for (count = 1; count < 65000; count = count * 2) {
	while (MTestGetDatatypes( &sendtype, &recvtype, count )) {
	    
	    sendtype.InitBuf( &sendtype );
	    recvtype.InitBuf( &recvtype );
	    
	    err = MPI_Irecv( recvtype.buf, recvtype.count, 
			    recvtype.datatype, rank, 0, comm, &req );
	    if (err) {
		errs++;
		if (errs < 10) {
		    MTestPrintError( err );
		}
	    }
	    
	    err = MPI_Send( sendtype.buf, sendtype.count, 
			    sendtype.datatype, rank, 0, comm);
	    if (err) {
		errs++;
		if (errs < 10) {
		    MTestPrintError( err );
		}
	    }
	    err = MPI_Wait( &req, MPI_STATUS_IGNORE );
	    err = MTestCheckRecv( 0, &recvtype );
	    if (err) {
		if (errs < 10) {
		    printf( "Data in target buffer did not match for destination datatype %s and source datatype %s, count = %d\n", 
			    MTestGetDatatypeName( &recvtype ),
			    MTestGetDatatypeName( &sendtype ),
			    count );
		    recvtype.printErrors = 1;
		    (void)MTestCheckRecv( 0, &recvtype );
		}
		errs += err;
	    }

	    err = MPI_Irecv( recvtype.buf, recvtype.count, 
			    recvtype.datatype, rank, 0, comm, &req );
	    if (err) {
		errs++;
		if (errs < 10) {
		    MTestPrintError( err );
		}
	    }
	    
	    err = MPI_Ssend( sendtype.buf, sendtype.count, 
			     sendtype.datatype, rank, 0, comm);
	    if (err) {
		errs++;
		if (errs < 10) {
		    MTestPrintError( err );
		}
	    }
	    err = MPI_Wait( &req, MPI_STATUS_IGNORE );
	    err = MTestCheckRecv( 0, &recvtype );
	    if (err) {
		if (errs < 10) {
		    printf( "Data in target buffer did not match for destination datatype %s and source datatype %s, count = %d\n", 
			    MTestGetDatatypeName( &recvtype ),
			    MTestGetDatatypeName( &sendtype ),
			    count );
		    recvtype.printErrors = 1;
		    (void)MTestCheckRecv( 0, &recvtype );
		}
		errs += err;
	    }

	    err = MPI_Irecv( recvtype.buf, recvtype.count, 
			    recvtype.datatype, rank, 0, comm, &req );
	    if (err) {
		errs++;
		if (errs < 10) {
		    MTestPrintError( err );
		}
	    }
	    
	    err = MPI_Rsend( sendtype.buf, sendtype.count, 
			     sendtype.datatype, rank, 0, comm);
	    if (err) {
		errs++;
		if (errs < 10) {
		    MTestPrintError( err );
		}
	    }
	    err = MPI_Wait( &req, MPI_STATUS_IGNORE );
	    err = MTestCheckRecv( 0, &recvtype );
	    if (err) {
		if (errs < 10) {
		    printf( "Data in target buffer did not match for destination datatype %s and source datatype %s, count = %d\n", 
			    MTestGetDatatypeName( &recvtype ),
			    MTestGetDatatypeName( &sendtype ),
			    count );
		    recvtype.printErrors = 1;
		    (void)MTestCheckRecv( 0, &recvtype );
		}
		errs += err;
	    }

	    MTestFreeDatatype( &sendtype );
	    MTestFreeDatatype( &recvtype );
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}