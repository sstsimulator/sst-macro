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

namespace baseattr2 {

void MissingKeyval( int rc, const char keyname[] );

int baseattr2( int argc, char **argv)
{
    int    errs = 0;
    int    rc;
    void *v;
    int  flag;
    int  vval;
    int  rank, size;

    MTest_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    /** Set errors return so that we can provide better information 
       should a routine reject one of the attribute values */
    MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_TAG_UB, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_TAG_UB" );
	errs++;
    }
    else {
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
    }

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_HOST, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_HOST" );
	errs++;
    }
    else {
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
    }

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_IO, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_IO" );
	errs++;
    }
    else {
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
    }

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_WTIME_IS_GLOBAL, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_WTIME_IS_GLOBAL" );
	errs++;
    }
    else {
	if (flag) {
	    /** Wtime need not be set */
	    vval = *(int*)v;
	    if (vval < 0 || vval > 1) {
		errs++;
		fprintf( stderr, "Invalid value for WTIME_IS_GLOBAL (got %d)\n", 
			 vval );
	    }
	}
    }

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_APPNUM, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_APPNUM" );
	errs++;
    }
    else {
	/** appnum need not be set */
	if (flag) {
	    vval = *(int *)v;
	    if (vval < 0) {
		errs++;
		fprintf( stderr, "MPI_APPNUM is defined as %d but must be nonnegative\n", vval );
	    }
	}
    }

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_UNIVERSE_SIZE, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_UNIVERSE_SIZE" );
	errs++;
    }
    else {
	/** MPI_UNIVERSE_SIZE need not be set */
	if (flag) {
	    vval = *(int *)v;
	    if (vval < size) {
		errs++;
		fprintf( stderr, "MPI_UNIVERSE_SIZE = %d, less than comm world (%d)\n", vval, size );
	    }
	}
    }

    rc = MPI_Attr_get( MPI_COMM_WORLD, MPI_LASTUSEDCODE, &v, &flag );
    if (rc) {
	MissingKeyval( rc, "MPI_LASTUSEDCODE" );
	errs++;
    }
    else {
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
    }

    MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL );

    MTest_Finalize( errs );
    MPI_Finalize( );
    
    return 0;
}

void MissingKeyval( int errcode, const char keyname[] )
{
    int errclass, slen;
    char string[MPI_MAX_ERROR_STRING];
    
    MPI_Error_class( errcode, &errclass );
    MPI_Error_string( errcode, string, &slen );
    printf( "For key %s: Error class %d (%s)\n", keyname, errclass, string );
    fflush( stdout );
}

}