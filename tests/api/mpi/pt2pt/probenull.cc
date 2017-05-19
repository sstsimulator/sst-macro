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

#include <stdio.h>
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace probenull {
/** 
 * This program checks that MPI_Iprobe and MPI_Probe correctly handle
 * a source of MPI_PROC_NULL
 */

int probenull(int argc, char **argv)
{
    int flag;
    int errs = 0;
    MPI_Status status;

    MTest_Init(&argc, &argv);

    MPI_Iprobe( MPI_PROC_NULL, 10, MPI_COMM_WORLD, &flag, &status );
    if (!flag) {
	errs++;
	printf( "Iprobe of source=MPI_PROC_NULL returned flag=false\n" );
    }
    else {
	if (status.MPI_SOURCE != MPI_PROC_NULL) {
	    printf( "Status.MPI_SOURCE was %d, should be MPI_PROC_NULL\n",
		    status.MPI_SOURCE );
	    errs++;
	}
	if (status.MPI_TAG    != MPI_ANY_TAG) {
	    printf( "Status.MPI_TAG was %d, should be MPI_ANY_TAGL\n",
		    status.MPI_TAG );
	    errs++;
	}
    }
    /** If Iprobe failed, probe is likely to as well.  Avoid a possible hang 
       by testing Probe only if Iprobe test passed */
    if (errs == 0) {
	MPI_Probe(  MPI_PROC_NULL, 10, MPI_COMM_WORLD, &status );
	if (status.MPI_SOURCE != MPI_PROC_NULL) {
	    printf( "Status.MPI_SOURCE was %d, should be MPI_PROC_NULL\n",
		    status.MPI_SOURCE );
	    errs++;
	}
	if (status.MPI_TAG    != MPI_ANY_TAG) {
	    printf( "Status.MPI_TAG was %d, should be MPI_ANY_TAGL\n",
		    status.MPI_TAG );
	    errs++;
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}