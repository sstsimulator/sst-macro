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

namespace infodup {
int infodup( int argc, char *argv[] )
{
    int errs = 0;
    MPI_Info info1, infodup;
    int nkeys, nkeysdup, i, vallen, flag, flagdup;
    char key[MPI_MAX_INFO_KEY], keydup[MPI_MAX_INFO_KEY];
    char value[MPI_MAX_INFO_VAL], valdup[MPI_MAX_INFO_VAL];

    MTest_Init( &argc, &argv );

    MPI_Info_create( &info1 );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    MPI_Info_set( info1, (char*)"host", (char*)"myhost.myorg.org" );
    MPI_Info_set( info1, (char*)"file", (char*)"runfile.txt" );
    MPI_Info_set( info1, (char*)"soft", (char*)"2:1000:4,3:1000:7" );

    MPI_Info_dup( info1, &infodup );

    MPI_Info_get_nkeys( infodup, &nkeysdup );
    MPI_Info_get_nkeys( info1, &nkeys );
    if (nkeys != nkeysdup) {
	errs++;
	printf( "Dup'ed info has a different number of keys; is %d should be %d\n",
		nkeysdup, nkeys );
    }
    vallen = MPI_MAX_INFO_VAL;
    for (i=0; i<nkeys; i++) {
	/** MPI requires that the keys are in the same order after the dup */
	MPI_Info_get_nthkey( info1, i, key );
	MPI_Info_get_nthkey( infodup, i, keydup );
	if (strcmp(key, keydup)) {
	    errs++;
	    printf( "keys do not match: %s should be %s\n", keydup, key );
	}

	vallen = MPI_MAX_INFO_VAL;
	MPI_Info_get( info1, key, vallen, value, &flag );
	MPI_Info_get( infodup, keydup, vallen, valdup, &flagdup );
	if (!flag || !flagdup) {
	    errs++;
	    printf( "Info get failed for key %s\n", key );
	}
	else if (strcmp( value, valdup )) {
	    errs++;
	    printf( "Info values for key %s not the same after dup\n", key );
	}
    }

    /** Change info and check that infodup does NOT have the new value 
       (ensure that lazy dups are still duped) */
    MPI_Info_set( info1, (char*)"path", (char*)"/a:/b:/c/d" );

    MPI_Info_get( infodup, (char*)"path", vallen, value, &flag );
    if (flag) {
	errs++;
	printf( "inserting path into info changed infodup\n" );
    }
    
    MPI_Info_free( &info1 );
    MPI_Info_free( &infodup );
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}