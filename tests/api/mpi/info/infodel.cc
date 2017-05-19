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

namespace infodel {

#define NKEYS 3
int infodel( int argc, char *argv[] )
{
    int errs = 0;
    MPI_Info info;
    char *keys[NKEYS] = { (char*)"file", (char*)"soft", (char*)"host" };
    char *values[NKEYS] = { (char*)"runfile.txt", (char*)"2:1000:4,3:1000:7", 
			    (char*)"myhost.myorg.org" };
    char value[MPI_MAX_INFO_VAL];
    int i, flag, nkeys;

    MTest_Init( &argc, &argv );

    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_set( info, keys[i], values[i] );
    }

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) { 
	MPI_Info_get( info, keys[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys[i] );
	}
	if (strcmp( value, values[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s, got %s expected %s\n", 
		    keys[i], value, values[i] );
	}
    }

    /** Now, change one value and remove another, then check again */
    MPI_Info_delete( info, keys[NKEYS-1] );
    MPI_Info_get_nkeys( info, &nkeys );
    if (nkeys != NKEYS - 1) {
	errs++;
	printf( "Deleting a key did not change the number of keys\n" );
    }

    values[0] = (char*)"backfile.txt";
    MPI_Info_set( info, keys[0], values[0] );
    for (i=0; i<NKEYS-1; i++) {
	MPI_Info_get( info, keys[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "(after reset) No value for key %s\n", keys[i] );
	}
	if (strcmp( value, values[i] )) {
	    errs++;
	    printf( "(after reset) Incorrect value for key %s, got %s expected %s\n", 
		    keys[i], value, values[i] );
	}
    }

    MPI_Info_free( &info );
    if (info != MPI_INFO_NULL) {
	errs++;
	printf( "MPI_Info_free should set info to MPI_INFO_NULL\n" );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}