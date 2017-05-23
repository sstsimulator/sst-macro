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

namespace commname {
int commname( int argc, char *argv[] )
{
    int errs = 0;
    MPI_Comm comm;
    int cnt, rlen;
    char name[MPI_MAX_OBJECT_NAME], nameout[MPI_MAX_OBJECT_NAME];
    MTest_Init( &argc, &argv );

    /** Check world and self firt */
    nameout[0] = 0;
    MPI_Comm_get_name( MPI_COMM_WORLD, nameout, &rlen );
    if (strcmp(nameout,"MPI_COMM_WORLD")) {
	errs++;
	printf( "Name of comm world is %s, should be MPI_COMM_WORLD\n", 
		nameout );
    }

    nameout[0] = 0;
    MPI_Comm_get_name( MPI_COMM_SELF, nameout, &rlen );
    if (strcmp(nameout,"MPI_COMM_SELF")) {
	errs++;
	printf( "Name of comm self is %s, should be MPI_COMM_SELF\n", 
		nameout );
    }

    /** Now, handle other communicators, including world/self */
    cnt = 0;
    while (MTestGetComm( &comm, 1 )) {
	if (comm == MPI_COMM_NULL) continue;
    
	sprintf( name, "comm-%d", cnt );
	cnt++;
	MPI_Comm_set_name( comm, name );
	nameout[0] = 0;
	MPI_Comm_get_name( comm, nameout, &rlen );
	if (strcmp( name, nameout )) {
	    errs++;
	    printf( "Unexpected name, was %s but should be %s\n",
		    nameout, name );
	}
	
	MTestFreeComm( &comm );
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}