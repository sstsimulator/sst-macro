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

namespace infoorder {

#define NKEYS 3
int infoorder( int argc, char *argv[] )
{
    int errs = 0;
    MPI_Info info;
    char *keys1[NKEYS] = { (char*)"file", (char*)"soft", (char*)"host" };
    char *values1[NKEYS] = { (char*)"runfile.txt", (char*)"2:1000:4,3:1000:7", 
			     (char*)"myhost.myorg.org" };

    char value[MPI_MAX_INFO_VAL];
    int i, flag;

    MTest_Init( &argc, &argv );

    /** 1,2,3 */
    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_set( info, keys1[i], values1[i] );
    }

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_get( info, keys1[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys1[i] );
	}
	if (strcmp( value, values1[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s\n", keys1[i] );
	}
    }
    MPI_Info_free( &info );

    /** 3,2,1 */
    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    for (i=NKEYS-1; i>=0; i--) {
	MPI_Info_set( info, keys1[i], values1[i] );
    }

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_get( info, keys1[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys1[i] );
	}
	if (strcmp( value, values1[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s\n", keys1[i] );
	}
    }
    MPI_Info_free( &info );

    /** 1,3,2 */
    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    MPI_Info_set( info, keys1[0], values1[0] );
    MPI_Info_set( info, keys1[2], values1[2] );
    MPI_Info_set( info, keys1[1], values1[1] );

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_get( info, keys1[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys1[i] );
	}
	if (strcmp( value, values1[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s\n", keys1[i] );
	}
    }
    MPI_Info_free( &info );

    /** 2,1,3 */
    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    MPI_Info_set( info, keys1[1], values1[1] );
    MPI_Info_set( info, keys1[0], values1[0] );
    MPI_Info_set( info, keys1[2], values1[2] );

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_get( info, keys1[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys1[i] );
	}
	if (strcmp( value, values1[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s\n", keys1[i] );
	}
    }
    MPI_Info_free( &info );

    /** 2,3,1 */
    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    MPI_Info_set( info, keys1[1], values1[1] );
    MPI_Info_set( info, keys1[2], values1[2] );
    MPI_Info_set( info, keys1[0], values1[0] );

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_get( info, keys1[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys1[i] );
	}
	if (strcmp( value, values1[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s\n", keys1[i] );
	}
    }
    MPI_Info_free( &info );
    
    /** 3,1,2 */
    MPI_Info_create( &info );
    /** Use only named keys incase the info implementation only supports
       the predefined keys (e.g., IBM) */
    MPI_Info_set( info, keys1[2], values1[2] );
    MPI_Info_set( info, keys1[0], values1[0] );
    MPI_Info_set( info, keys1[1], values1[1] );

    /** Check that all values are present */
    for (i=0; i<NKEYS; i++) {
	MPI_Info_get( info, keys1[i], MPI_MAX_INFO_VAL, value, &flag );
	if (!flag) {
	    errs++;
	    printf( "No value for key %s\n", keys1[i] );
	}
	if (strcmp( value, values1[i] )) {
	    errs++;
	    printf( "Incorrect value for key %s\n", keys1[i] );
	}
    }
    MPI_Info_free( &info );
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

}