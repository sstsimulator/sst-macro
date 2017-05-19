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
#include <string.h>
#include "mpitest.h"

namespace opmaxloc {
/**
static char MTEST_Descrip[] = "Test MPI_MAXLOC operations on datatypes dupported by MPICH2";
*/

/**
 * This test looks at the handling of char and types that  are not required 
 * integers (e.g., long long).  MPICH2 allows
 * these as well.  A strict MPI test should not include this test.
 *
 * The rule on max loc is that if there is a tie in the value, the minimum
 * rank is used (see 4.9.3 in the MPI-1 standard)
 */
int opmaxloc( int argc, char *argv[] )
{
    int errs = 0;
    int rank, size;
    MPI_Comm      comm;

    MTest_Init( &argc, &argv );

    comm = MPI_COMM_WORLD;

    MPI_Comm_rank( comm, &rank );
    MPI_Comm_size( comm, &size );

    /** 2 int */
    {
	struct twoint { int val; int loc; } cinbuf[3], coutbuf[3];
 	
	cinbuf[0].val = 1;
	cinbuf[0].loc = rank;
	cinbuf[1].val = 0;
	cinbuf[1].loc = rank;
	cinbuf[2].val = rank;
	cinbuf[2].loc = rank;
	
	coutbuf[0].val = 0;
	coutbuf[0].loc = -1;
	coutbuf[1].val = 1;
	coutbuf[1].loc = -1;
	coutbuf[2].val = 1;
	coutbuf[2].loc = -1;
	MPI_Reduce( cinbuf, coutbuf, 3, MPI_2INT, MPI_MAXLOC, 0, comm );
	if (rank == 0) {
	    if (coutbuf[0].val != 1 || coutbuf[0].loc != 0) {
		errs++;
		fprintf( stderr, "2int MAXLOC(1) test failed\n" );
	    }
	    if (coutbuf[1].val != 0) {
		errs++;
		fprintf( stderr, "2int MAXLOC(0) test failed, value = %d, should be zero\n", coutbuf[1].val );
	    }
	    if (coutbuf[1].loc != 0) {
		errs++;
		fprintf( stderr, "2int MAXLOC(0) test failed, location of max = %d, should be zero\n", coutbuf[1].loc );
	    }
	    if (coutbuf[2].val != size-1 || coutbuf[2].loc != size-1) {
		errs++;
		fprintf( stderr, "2int MAXLOC(>) test failed\n" );
	    }
	}
    }

    /** float int */
    {
	struct floatint { float val; int loc; } cinbuf[3], coutbuf[3];
 	
	cinbuf[0].val = 1;
	cinbuf[0].loc = rank;
	cinbuf[1].val = 0;
	cinbuf[1].loc = rank;
	cinbuf[2].val = (float)rank;
	cinbuf[2].loc = rank;
	
	coutbuf[0].val = 0;
	coutbuf[0].loc = -1;
	coutbuf[1].val = 1;
	coutbuf[1].loc = -1;
	coutbuf[2].val = 1;
	coutbuf[2].loc = -1;
	MPI_Reduce( cinbuf, coutbuf, 3, MPI_FLOAT_INT, MPI_MAXLOC, 0, comm );
	if (rank == 0) {
	    if (coutbuf[0].val != 1 || coutbuf[0].loc != 0) {
		errs++;
		fprintf( stderr, "float-int MAXLOC(1) test failed\n" );
	    }
	    if (coutbuf[1].val != 0) {
		errs++;
		fprintf( stderr, "float-int MAXLOC(0) test failed, value = %f, should be zero\n", coutbuf[1].val );
	    }
	    if (coutbuf[1].loc != 0) {
		errs++;
		fprintf( stderr, "float-int MAXLOC(0) test failed, location of max = %d, should be zero\n", coutbuf[1].loc );
	    }
	    if (coutbuf[2].val != size-1 || coutbuf[2].loc != size-1) {
		errs++;
		fprintf( stderr, "float-int MAXLOC(>) test failed\n" );
	    }
	}
    }
    
    /** long int */
    {
	struct longint { long val; int loc; } cinbuf[3], coutbuf[3];
 	
	cinbuf[0].val = 1;
	cinbuf[0].loc = rank;
	cinbuf[1].val = 0;
	cinbuf[1].loc = rank;
	cinbuf[2].val = rank;
	cinbuf[2].loc = rank;
	
	coutbuf[0].val = 0;
	coutbuf[0].loc = -1;
	coutbuf[1].val = 1;
	coutbuf[1].loc = -1;
	coutbuf[2].val = 1;
	coutbuf[2].loc = -1;
	MPI_Reduce( cinbuf, coutbuf, 3, MPI_LONG_INT, MPI_MAXLOC, 0, comm );
	if (rank == 0) {
	    if (coutbuf[0].val != 1 || coutbuf[0].loc != 0) {
		errs++;
		fprintf( stderr, "long-int MAXLOC(1) test failed\n" );
	    }
	    if (coutbuf[1].val != 0) {
		errs++;
		fprintf( stderr, "long-int MAXLOC(0) test failed, value = %ld, should be zero\n", coutbuf[1].val );
	    }
	    if (coutbuf[1].loc != 0) {
		errs++;
		fprintf( stderr, "long-int MAXLOC(0) test failed, location of max = %d, should be zero\n", coutbuf[1].loc );
	    }
	    if (coutbuf[2].val != size-1 || coutbuf[2].loc != size-1) {
		errs++;
		fprintf( stderr, "long-int MAXLOC(>) test failed\n" );
	    }
	}
    }

    /** short int */
    {
	struct shortint { short val; int loc; } cinbuf[3], coutbuf[3];
 	
	cinbuf[0].val = 1;
	cinbuf[0].loc = rank;
	cinbuf[1].val = 0;
	cinbuf[1].loc = rank;
	cinbuf[2].val = rank;
	cinbuf[2].loc = rank;
	
	coutbuf[0].val = 0;
	coutbuf[0].loc = -1;
	coutbuf[1].val = 1;
	coutbuf[1].loc = -1;
	coutbuf[2].val = 1;
	coutbuf[2].loc = -1;
	MPI_Reduce( cinbuf, coutbuf, 3, MPI_SHORT_INT, MPI_MAXLOC, 0, comm );
	if (rank == 0) {
	    if (coutbuf[0].val != 1 || coutbuf[0].loc != 0) {
		errs++;
		fprintf( stderr, "short-int MAXLOC(1) test failed\n" );
	    }
	    if (coutbuf[1].val != 0) {
		errs++;
		fprintf( stderr, "short-int MAXLOC(0) test failed, value = %d, should be zero\n", coutbuf[1].val );
	    }
	    if (coutbuf[1].loc != 0) {
		errs++;
		fprintf( stderr, "short-int MAXLOC(0) test failed, location of max = %d, should be zero\n", coutbuf[1].loc );
	    }
	    if (coutbuf[2].val != size-1) {
		errs++;
		fprintf( stderr, "short-int MAXLOC(>) test failed, value = %d, should be %d\n", coutbuf[2].val, size-1 );
	    }
	    if (coutbuf[2].loc != size -1) {
		errs++;
		fprintf( stderr, "short-int MAXLOC(>) test failed, location of max = %d, should be %d\n", coutbuf[2].loc, size-1 );
	    }
	}
    }
    
    /** double int */
    {
	struct doubleint { double val; int loc; } cinbuf[3], coutbuf[3];
 	
	cinbuf[0].val = 1;
	cinbuf[0].loc = rank;
	cinbuf[1].val = 0;
	cinbuf[1].loc = rank;
	cinbuf[2].val = rank;
	cinbuf[2].loc = rank;
	
	coutbuf[0].val = 0;
	coutbuf[0].loc = -1;
	coutbuf[1].val = 1;
	coutbuf[1].loc = -1;
	coutbuf[2].val = 1;
	coutbuf[2].loc = -1;
	MPI_Reduce( cinbuf, coutbuf, 3, MPI_DOUBLE_INT, MPI_MAXLOC, 0, comm );
	if (rank == 0) {
	    if (coutbuf[0].val != 1 || coutbuf[0].loc != 0) {
		errs++;
		fprintf( stderr, "double-int MAXLOC(1) test failed\n" );
	    }
	    if (coutbuf[1].val != 0) {
		errs++;
		fprintf( stderr, "double-int MAXLOC(0) test failed, value = %lf, should be zero\n", coutbuf[1].val );
	    }
	    if (coutbuf[1].loc != 0) {
		errs++;
		fprintf( stderr, "double-int MAXLOC(0) test failed, location of max = %d, should be zero\n", coutbuf[1].loc );
	    }
	    if (coutbuf[2].val != size-1 || coutbuf[2].loc != size-1) {
		errs++;
		fprintf( stderr, "double-int MAXLOC(>) test failed\n" );
	    }
	}
    }
    
#ifdef HAVE_LONG_DOUBLE
    /** long double int */
    {
	struct longdoubleint { long double val; int loc; } cinbuf[3], coutbuf[3];

        /** avoid valgrind warnings about padding bytes in the long double */
        memset(&cinbuf[0], 0, sizeof(cinbuf));
        memset(&coutbuf[0], 0, sizeof(coutbuf));

	cinbuf[0].val = 1;
	cinbuf[0].loc = rank;
	cinbuf[1].val = 0;
	cinbuf[1].loc = rank;
	cinbuf[2].val = rank;
	cinbuf[2].loc = rank;
	
	coutbuf[0].val = 0;
	coutbuf[0].loc = -1;
	coutbuf[1].val = 1;
	coutbuf[1].loc = -1;
	coutbuf[2].val = 1;
	coutbuf[2].loc = -1;
	if (MPI_LONG_DOUBLE != MPI_DATATYPE_NULL) {
	    MPI_Reduce( cinbuf, coutbuf, 3, MPI_LONG_DOUBLE_INT, MPI_MAXLOC, 
			0, comm );
	    if (rank == 0) {
		if (coutbuf[0].val != 1 || coutbuf[0].loc != 0) {
		    errs++;
		    fprintf( stderr, "long double-int MAXLOC(1) test failed\n" );
		}
		if (coutbuf[1].val != 0) {
		    errs++;
		    fprintf( stderr, "long double-int MAXLOC(0) test failed, value = %lf, should be zero\n", (double)coutbuf[1].val );
		}
		if (coutbuf[1].loc != 0) {
		    errs++;
		    fprintf( stderr, "long double-int MAXLOC(0) test failed, location of max = %d, should be zero\n", coutbuf[1].loc );
		}
		if (coutbuf[2].val != size-1) {
		    errs++;
		    fprintf( stderr, "long double-int MAXLOC(>) test failed, value = %lf, should be %d\n", (double)coutbuf[2].val, size-1 );
		}
		if (coutbuf[2].loc != size-1) {
		    errs++;
		    fprintf( stderr, "long double-int MAXLOC(>) test failed, location of max = %d, should be %d\n", coutbuf[2].loc, size-1 );
		}
	    }
	}
    }
#endif

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}