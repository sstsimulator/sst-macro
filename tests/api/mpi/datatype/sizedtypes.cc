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

namespace sizedtypes {
/**
static char MTEST_Descrip[] = "Test of the sized types, supported in MPI-2";
*/

int sizedtypes( int argc, char *argv[] )
{
    int errs = 0;
    int size;

    MTest_Init( &argc, &argv );

    MPI_Type_size( MPI_REAL4, &size );
    if (size != 4) {
	errs ++;
	printf( "MPI_REAL4 has size %d\n", size );
    }
    MPI_Type_size( MPI_REAL8, &size );
    if (size != 8) {
	errs ++;
	printf( "MPI_REAL8 has size %d\n", size );
    }
    if (MPI_REAL16 != MPI_DATATYPE_NULL) {
	MPI_Type_size( MPI_REAL16, &size );
	if (size != 16) {
	    errs ++;
	    printf( "MPI_REAL16 has size %d\n", size );
	}
    }

    MPI_Type_size( MPI_COMPLEX8, &size );
    if (size != 8) {
	errs ++;
	printf( "MPI_COMPLEX8 has size %d\n", size );
    }
    MPI_Type_size( MPI_COMPLEX16, &size );
    if (size != 16) {
	errs ++;
	printf( "MPI_COMPLEX16 has size %d\n", size );
    }
    if (MPI_COMPLEX32 != MPI_DATATYPE_NULL) {
	MPI_Type_size( MPI_COMPLEX32, &size );
	if (size != 32) {
	    errs ++;
	    printf( "MPI_COMPLEX32 has size %d\n", size );
	}
    }

    MPI_Type_size( MPI_INTEGER1, &size );
    if (size != 1) {
	errs ++;
	printf( "MPI_INTEGER1 has size %d\n", size );
    }
    MPI_Type_size( MPI_INTEGER2, &size );
    if (size != 2) {
	errs ++;
	printf( "MPI_INTEGER2 has size %d\n", size );
    }
    MPI_Type_size( MPI_INTEGER4, &size );
    if (size != 4) {
	errs ++;
	printf( "MPI_INTEGER4 has size %d\n", size );
    }
    if (MPI_INTEGER8 != MPI_DATATYPE_NULL) {
	MPI_Type_size( MPI_INTEGER8, &size );
	if (size != 8) {
	    errs ++;
	    printf( "MPI_INTEGER8 has size %d\n", size );
	}
    }
#ifdef HAVE_MPI_INTEGER16
    if (MPI_INTEGER16 != MPI_DATATYPE_NULL) {
	MPI_Type_size( MPI_INTEGER16, &size );
	if (size != 16) {
	    errs ++;
	    printf( "MPI_INTEGER16 has size %d\n", size );
	}
    }
#endif

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}