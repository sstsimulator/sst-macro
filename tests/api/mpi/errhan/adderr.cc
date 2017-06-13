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
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace adderr {

/** Create NCLASSES new classes, each with 5 codes (160 total) */
#define NCLASSES 32
#define NCODES   5

int adderr( int argc, char *argv[] )
{
    int errs = 0;
    char string[MPI_MAX_ERROR_STRING], outstring[MPI_MAX_ERROR_STRING];
    int newclass[NCLASSES], newcode[NCLASSES][NCODES];
    int i, j, slen, outclass;

    MTest_Init( &argc, &argv );

    /** Initialize the new codes */
    for (i=0; i<NCLASSES; i++) {
	MPI_Add_error_class( &newclass[i] );
	for (j=0; j<NCODES; j++) {
	    MPI_Add_error_code( newclass[i], &newcode[i][j] );
	    sprintf( string, "code for class %d code %d\n", i, j );
	    MPI_Add_error_string( newcode[i][j], string );
	}
    }

    /** check the values */
    for (i=0; i<NCLASSES; i++) {
	MPI_Error_class( newclass[i], &outclass );
	if (outclass != newclass[i]) {
	    errs++;
	    printf( "Error class %d is not a valid error code %x %x\n", i,
		    outclass, newclass[i]);
	}
	for (j=0; j<NCODES; j++) {
	    MPI_Error_class( newcode[i][j], &outclass );
	    if (outclass != newclass[i]) {
		errs++;
		printf( "Class of code for %d is not correct %x %x\n", j,
			outclass, newclass[i] );
	    }
	    MPI_Error_string( newcode[i][j], outstring, &slen );
	    sprintf( string, "code for class %d code %d\n", i, j );
	    if (strcmp( outstring, string )) {
		errs++;
		printf( "Error string is :%s: but should be :%s:\n",
			outstring, string );
	    }
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}