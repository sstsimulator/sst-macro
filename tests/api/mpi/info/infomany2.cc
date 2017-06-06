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

namespace infomany2 {
#ifndef MAX_INFOS
#define MAX_INFOS 4000
#endif
#define MAX_ERRORS 10
#define info_list 16
/** #define DBG  */

#ifdef DEBUG
#define DBGPRINTF(a) printf a; fflush(stdout)
#else
#define DBGPRINTF(a)
#endif

int infomany2( int argc, char *argv[] )
{
    MPI_Info infos[MAX_INFOS];
    char key[64], value[64];
    int  errs = 0;
    int  i, j;

    MTest_Init( &argc, &argv );

    /** We create max_info items, then delete the middle third of them,
       then recreate them, then check them, then 
       delete them all.  This checks that the MPICH2 algorithm for 
       handling large numbers of items works correctly; other MPI 
       implementations should also be able to handle this */

    /** Create them all */
    for (i=0; i<MAX_INFOS; i++) {
	MPI_Info_create( &infos[i] );
	DBGPRINTF( ( "Info handle is %x\n", infos[i] ) );
	for (j=0; j<info_list; j++) {
	    sprintf( key, "key%d-%d", i, j );
	    sprintf( value, "value%d-%d", i, j );
	    DBGPRINTF( ( "Creating key/value %s=%s\n", key, value ));
	    MPI_Info_set( infos[i], key, value );
	}
#ifdef DBG
	{ int nkeys;
	MPI_Info_get_nkeys( infos[0], &nkeys );
	if (nkeys != info_list) {
	    printf( "infos[0] changed at %d info\n", i );}
	}
#endif
    }

    /** Delete the middle set */
    for (i=MAX_INFOS/3; i<(2*MAX_INFOS/3); i++) {
	MPI_Info_free( &infos[i] );
    }
    
    /** Recreate the middle set */
    for (i=MAX_INFOS/3; i<(2*MAX_INFOS/3); i++) {
	MPI_Info_create( &infos[i] );
	DBGPRINTF( ( "Info handle is %x\n", infos[i] ) );
	for (j=0; j<info_list; j++) {
	    sprintf( key, "key%d-%d", i, j );
	    sprintf( value, "value%d-%d", i, j );
	    DBGPRINTF( ( "Creating key/value %s=%s\n", key, value ));
	    MPI_Info_set( infos[i], key, value );
	}
    }

    /** Now, check that they are still valid */
    for (i=0; i<MAX_INFOS; i++) {
	int nkeys;
	/**printf( "info = %x\n", infos[i] );
	  print_handle( infos[i] ); printf( "\n" );*/
	MPI_Info_get_nkeys( infos[i], &nkeys );
	if (nkeys != info_list) {
	    errs++;
	    if (errs < MAX_ERRORS) {
		printf( "Wrong number of keys for info %d; got %d, should be %d\n",
			i, nkeys, info_list );
	    }
	}
	for (j=0; j<nkeys; j++) {
	    char keystr[64];
	    char valstr[64];
	    int  flag;
	    MPI_Info_get_nthkey( infos[i], j, key );
	    sprintf( keystr, "key%d-%d", i, j );
	    if (strcmp( keystr, key ) != 0) {
		errs++;
		if (errs < MAX_ERRORS) {
		    printf( "Wrong key for info %d; got %s expected %s\n", 
			    i, key, keystr );
		}
		continue;
	    }
	    MPI_Info_get( infos[i], key, 64, value, &flag );
	    if (!flag) {
		errs++;
		if (errs < MAX_ERRORS) {
		    printf( "Get failed to return value for info %d\n", i );
		}
		continue;
	    }
	    sprintf( valstr, "value%d-%d", i, j );
	    if (strcmp( valstr, value ) != 0) {
		errs++;
		if (errs < MAX_ERRORS) {
		    printf( "Wrong value for info %d; got %s expected %s\n",
			    i, value, valstr );
		}
	    }
	}
    }
    for (i=0; i<MAX_INFOS; i++) {
	MPI_Info_free( &infos[i] );
    }
    
    MTest_Finalize( errs );
    MPI_Finalize( );
    return 0;
}

}