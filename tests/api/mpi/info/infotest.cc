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
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace infotest {

int infotest( int argc, char *argv[] )
{
    MPI_Info i1, i2;
    int errs = 0;
    char value[64];
    int flag;

    MPI_Init( 0, 0 );
    
    MPI_Info_create( &i1 );
    MPI_Info_create( &i2 );

    MPI_Info_set( i1, (char*)"key1", (char*)"value1" );
    MPI_Info_set( i2, (char*)"key2", (char*)"value2" );

    MPI_Info_get( i1, (char*)"key2", 64, value, &flag );
    if (flag) {
	printf( "Found key2 in info1\n" );
	errs ++;
    }
    MPI_Info_get( i1, (char*)"key1", 64, value, &flag );
    if (!flag) {
	errs++;
	printf( "Did not find key1 in info1\n" );
    }
    else if (strcmp( value, "value1" )) {
	errs++;
	printf( "Found wrong value (%s), expected value1\n", value );
    }

    MPI_Info_free( &i1 );
    MPI_Info_free( &i2 );
    if (errs) {
	printf( " Found %d errors\n", errs );
    }
    else {
	printf( " No Errors\n" );
    }
    MPI_Finalize( );
    return 0;
}

}