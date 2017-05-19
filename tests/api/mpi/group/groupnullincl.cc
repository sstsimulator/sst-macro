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

namespace groupnullincl {
int groupnullincl( int argc, char *argv[] )
{
    int errs = 0;
    int rc, result;
    int ranks[1];
    MPI_Group group, outgroup;
    MPI_Comm comm;

    MTest_Init( &argc, &argv );
    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    while (MTestGetComm( &comm, 1 )) {
	if (comm == MPI_COMM_NULL) continue;

	MPI_Comm_group( comm, &group );
	rc = MPI_Group_incl( group, 0, 0, &outgroup );
	if (rc) {
	    errs++;
	    MTestPrintError( rc );
	    printf( "Error in creating an empty group with (0,0)\n" );
	    
	    /** Some MPI implementations may reject a null "ranks" pointer */
	    rc = MPI_Group_incl( group, 0, ranks, &outgroup );
	    if (rc) {
		errs++;
		MTestPrintError( rc );
		printf( "Error in creating an empty group with (0,ranks)\n" );
	    }
	}

	if (outgroup != MPI_GROUP_EMPTY) {
	    /** Is the group equivalent to group empty? */
	    rc = MPI_Group_compare( outgroup, MPI_GROUP_EMPTY, &result );
	    if (result != MPI_IDENT) {
		errs++;
		MTestPrintError( rc );
		printf( "Did not create a group equivalent to an empty group\n" );
	    }
	}
	rc = MPI_Group_free( &group );
	if (rc) {
	    errs++;
	    MTestPrintError( rc );
	}	    
	if (outgroup != MPI_GROUP_NULL) {
	    rc = MPI_Group_free( &outgroup );
	    if (rc) {
		errs++;
		MTestPrintError( rc );
	    }
	}

	MTestFreeComm( &comm );
    }
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}
}