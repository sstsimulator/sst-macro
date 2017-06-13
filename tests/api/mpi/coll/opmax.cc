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
#include "mpitestconf.h"
#include <stdio.h>
#include "mpitest.h"

namespace opmax {
/**
static char MTEST_Descrip[] = "Test MPI_MAX operations on optional datatypes dupported by MPICH2";
*/

/**
 * This test looks at the handling of char and types that  are not required 
 * integers (e.g., long long).  MPICH2 allows
 * these as well.  A strict MPI test should not include this test.
 */
int opmax( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size;
    MPI_Comm      comm;
    char cinbuf[3], coutbuf[3];
    signed char scinbuf[3], scoutbuf[3];
    unsigned char ucinbuf[3], ucoutbuf[3];

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

#ifndef USE_STRICT_MPI
    /** char */
    MTestPrintfMsg( 10, "Reduce of MPI_CHAR\n" );
    cinbuf[0] = 1;
    cinbuf[1] = 0;
    cinbuf[2] = rank;

    coutbuf[0] = 0;
    coutbuf[1] = 1;
    coutbuf[2] = 1;
    MPI_Reduce( cinbuf, coutbuf, 3, MPI_CHAR, MPI_MAX, 0, comm );
    if (rank == 0) {
	if (coutbuf[0] != 1) {
	    errs++;
	    fprintf( stderr, "char MAX(1) test failed\n" );
	}
	if (coutbuf[1] != 0) {
	    errs++;
	    fprintf( stderr, "char MAX(0) test failed\n" );
	}
	if (size < 128 && coutbuf[2] != size - 1) {
	    errs++;
	    fprintf( stderr, "char MAX(>) test failed\n" );
	}
    }
#endif /** USE_STRICT_MPI */

    /** signed char */
    MTestPrintfMsg( 10, "Reduce of MPI_SIGNED_CHAR\n" );
    scinbuf[0] = 1;
    scinbuf[1] = 0;
    scinbuf[2] = rank;

    scoutbuf[0] = 0;
    scoutbuf[1] = 1;
    scoutbuf[2] = 1;
    MPI_Reduce( scinbuf, scoutbuf, 3, MPI_SIGNED_CHAR, MPI_MAX, 0, comm );
    if (rank == 0) {
	if (scoutbuf[0] != 1) {
	    errs++;
	    fprintf( stderr, "signed char MAX(1) test failed\n" );
	}
	if (scoutbuf[1] != 0) {
	    errs++;
	    fprintf( stderr, "signed char MAX(0) test failed\n" );
	}
	if (size < 128 && scoutbuf[2] != size - 1) {
	    errs++;
	    fprintf( stderr, "signed char MAX(>) test failed\n" );
	}
    }

    /** unsigned char */
    MTestPrintfMsg( 10, "Reduce of MPI_UNSIGNED_CHAR\n" );
    ucinbuf[0] = 1;
    ucinbuf[1] = 0;
    ucinbuf[2] = rank;

    ucoutbuf[0] = 0;
    ucoutbuf[1] = 1;
    ucoutbuf[2] = 1;
    MPI_Reduce( ucinbuf, ucoutbuf, 3, MPI_UNSIGNED_CHAR, MPI_MAX, 0, comm );
    if (rank == 0) {
	if (ucoutbuf[0] != 1) {
	    errs++;
	    fprintf( stderr, "unsigned char MAX(1) test failed\n" );
	}
	if (ucoutbuf[1]) {
	    errs++;
	    fprintf( stderr, "unsigned char MAX(0) test failed\n" );
	}
	if (size < 256 && ucoutbuf[2] != size - 1) {
	    errs++;
	    fprintf( stderr, "unsigned char MAX(>) test failed\n" );
	}
    }

#ifdef HAVE_LONG_DOUBLE
    { long double ldinbuf[3], ldoutbuf[3];
    /** long double */
    ldinbuf[0] = 1;
    ldinbuf[1] = 0;
    ldinbuf[2] = rank;

    ldoutbuf[0] = 0;
    ldoutbuf[1] = 1;
    ldoutbuf[2] = 1;
    if (MPI_LONG_DOUBLE != MPI_DATATYPE_NULL) {
	MTestPrintfMsg( 10, "Reduce of MPI_LONG_DOUBLE\n" );
	MPI_Reduce( ldinbuf, ldoutbuf, 3, MPI_LONG_DOUBLE, MPI_MAX, 0, comm );
	if (rank == 0) {
	    if (ldoutbuf[0] != 1) {
		errs++;
		fprintf( stderr, "long double MAX(1) test failed\n" );
	    }
	    if (ldoutbuf[1] != 0.0) {
		errs++;
		fprintf( stderr, "long double MAX(0) test failed\n" );
	    }
	    if (ldoutbuf[2] != size - 1) {
		errs++;
		fprintf( stderr, "long double MAX(>) test failed\n" );
	    }
	}
    }
    }
#endif /** HAVE_LONG_DOUBLE */

#ifdef HAVE_LONG_LONG
    {
	long long llinbuf[3], lloutbuf[3];
    /** long long */
    llinbuf[0] = 1;
    llinbuf[1] = 0;
    llinbuf[2] = rank;

    lloutbuf[0] = 0;
    lloutbuf[1] = 1;
    lloutbuf[2] = 1;
    if (MPI_LONG_LONG != MPI_DATATYPE_NULL) {
	MTestPrintfMsg( 10, "Reduce of MPI_LONG_LONG\n" );
	MPI_Reduce( llinbuf, lloutbuf, 3, MPI_LONG_LONG, MPI_MAX, 0, comm );
	if (rank == 0) {
	    if (lloutbuf[0] != 1) {
		errs++;
		fprintf( stderr, "long long MAX(1) test failed\n" );
	    }
	    if (lloutbuf[1] != 0) {
		errs++;
		fprintf( stderr, "long long MAX(0) test failed\n" );
	    }
	    if (lloutbuf[2] != size - 1) {
		errs++;
		fprintf( stderr, "long long MAX(>) test failed\n" );
	    }
	}
    }
    }
#endif /** HAVE_LONG_LONG */

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}