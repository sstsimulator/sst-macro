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

namespace bottom {
/**
static char MTEST_Descrip[] = "Use of MPI_BOTTOM in communication";
*/

int bottom( int argc, char *argv[] )
{
    int errs = 0, err;
    int rank, size, source, dest, len, ii;
    MPI_Comm      comm;
    MPI_Status    status;
    MPI_Datatype  newtype, oldtype;
    MPI_Aint      disp;

    MTest_Init( &argc, &argv );

    MPI_Get_address( &ii, &disp );

    len     = 1;
    oldtype = MPI_INT;
    MPI_Type_create_struct( 1, &len, &disp, &oldtype, &newtype );
    MPI_Type_commit( &newtype );

    comm = MPI_COMM_WORLD;

    MPI_Comm_size( comm, &size );
    MPI_Comm_rank( comm, &rank );

    if (size < 2) {
	errs++;
	fprintf( stderr, "This test requires at least two processes\n" );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    source = 0;
    dest = 1;

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( comm, MPI_ERRORS_RETURN );

    if (rank == source) {
	ii = 2;
	err = MPI_Send( MPI_BOTTOM, 1, newtype, dest, 0, comm );
	if (err) {
	    errs++;
	    MTestPrintError( err );
	    printf( "MPI_Send did not return MPI_SUCCESS\n" );
	}
    }
    else if (rank == dest) {
	ii = -1;
	err = MPI_Recv( MPI_BOTTOM, 1, newtype, source, 0, comm, &status );
	if (err) {
	    MTestPrintError( err );
	    errs++;
	    printf( "MPI_Recv did not return MPI_SUCCESS\n" );
	}
	if (ii != 2) {
	    errs++;
	    printf( "Received %d but expected %d\n", ii, 2 );
	}
    }

    MPI_Comm_set_errhandler( comm, MPI_ERRORS_ARE_FATAL );

    MPI_Type_free( &newtype );

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}