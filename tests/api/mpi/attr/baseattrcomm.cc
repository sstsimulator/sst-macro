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

namespace baseattrcomm {

int baseattrcomm( int argc, char **argv)
{
    int    errs = 0;
    void *v;
    int  flag;
    int  vval;
    int  rank, size;

    MTest_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_TAG_UB, &v, &flag );
    if (!flag) {
	errs++;
	fprintf( stderr, "Could not get TAG_UB\n" );
    }
    else {
	vval = *(int*)v;
	if (vval < 32767) {
	    errs++;
	    fprintf( stderr, "Got too-small value (%d) for TAG_UB\n", vval );
	}
    }

    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_HOST, &v, &flag );
    if (!flag) {
	errs++;
	fprintf( stderr, "Could not get HOST\n" );
    }
    else {
	vval = *(int*)v;
	if ((vval < 0 || vval >= size) && vval != MPI_PROC_NULL) {
	    errs++;
	    fprintf( stderr, "Got invalid value %d for HOST\n", vval );
	}
    }
    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_IO, &v, &flag );
    if (!flag) {
	errs++;
	fprintf( stderr, "Could not get IO\n" );
    }
    else {
	vval = *(int*)v;
	if ((vval < 0 || vval >= size) && vval != MPI_ANY_SOURCE &&
		  vval != MPI_PROC_NULL) {
	    errs++;
	    fprintf( stderr, "Got invalid value %d for IO\n", vval );
	}
    }

    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_WTIME_IS_GLOBAL, &v, &flag );
    if (flag) {
	/** Wtime need not be set */
	vval = *(int*)v;
	if (vval < 0 || vval > 1) {
	    errs++;
	    fprintf( stderr, "Invalid value for WTIME_IS_GLOBAL (got %d)\n", 
		     vval );
	}
    }

    /** MPI 2.0, section 5.5.3 - MPI_APPNUM should be set if the program is
       started with more than one executable name (e.g., in MPMD instead
       of SPMD mode).  This is independent of the dynamic process routines,
       and should be supported even if MPI_COMM_SPAWN and friends are not. */
    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_APPNUM, &v, &flag );
    /** appnum need not be set */
    if (flag) {
	vval = *(int *)v;
	if (vval < 0) {
	    errs++;
	    fprintf( stderr, "MPI_APPNUM is defined as %d but must be nonnegative\n", vval );
	}
    }

    /** MPI 2.0 section 5.5.1.  MPI_UNIVERSE_SIZE need not be set, but
       should be present.  */
    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_UNIVERSE_SIZE, &v, &flag );
    /** MPI_UNIVERSE_SIZE need not be set */
    if (flag) {
	/** But if it is set, it must be at least the size of comm_world */
	vval = *(int *)v;
	if (vval < size) {
	    errs++;
	    fprintf( stderr, "MPI_UNIVERSE_SIZE = %d, less than comm world (%d)\n", vval, size );
	}
    }
    
    MPI_Comm_get_attr( MPI_COMM_WORLD, MPI_LASTUSEDCODE, &v, &flag );
    /** Last used code must be defined and >= MPI_ERR_LASTCODE */
    if (flag) {
	vval = *(int*)v;
	if (vval < MPI_ERR_LASTCODE) {
	    errs++;
	    fprintf( stderr, "MPI_LASTUSEDCODE points to an integer (%d) smaller than MPI_ERR_LASTCODE (%d)\n", vval, MPI_ERR_LASTCODE );
	}
    }
    else {
	errs++;
	fprintf( stderr, "MPI_LASTUSECODE is not defined\n" );
    }

    MTest_Finalize( errs );
    MPI_Finalize( );
    
    return 0;
}

}