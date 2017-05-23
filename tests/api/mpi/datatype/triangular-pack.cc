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

#include <math.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace triangular_pack {

int triangular_pack(int argc, char *argv[]);

/** helper functions */
int parse_args(int argc, char **argv);

static int verbose = 0;

int triangular_pack(int argc, char *argv[])
{
    /** Variable declarations */
    int a[100][100], b[100][100];
    int disp[100], block[100];
    MPI_Datatype ltype;
	
    int bufsize, position = 0;
    void *buffer;
	
    int i, j, errs = 0;
	
    /** Initialize a to some known values and zero out b. */
    for(i = 0; i < 100; i++) {
	for(j = 0; j < 100; j++) {
	    a[i][j] = 1000*i + j;
	    b[i][j] = 0;
	}
    }
	
    /** Initialize MPI */
    MTest_Init( &argc, &argv );
  
    //parse_args(argc, argv);

    for(i = 0; i < 100; i++) {
	/** Fortran version has disp(i) = 100*(i-1) + i and block(i) = 100-i. */
	/** This code here is wrong. It compacts everything together,
	 * which isn't what we want.
	 * What we want is to put the lower triangular values into b and leave
	 * the rest of it unchanged, right?
	 */
	block[i] = i+1;
	disp[i] = 100*i;
    }
	
    /** Create datatype for lower triangular part. */
    MPI_Type_indexed(100, block, disp, MPI_INT, &ltype);
    MPI_Type_commit(&ltype);
	
    /** Pack it. */
    MPI_Pack_size(1, ltype, MPI_COMM_WORLD, &bufsize);
    buffer = (void *) malloc((unsigned) bufsize);
    MPI_Pack( a, 1, ltype, buffer, bufsize, &position, MPI_COMM_WORLD );
	
    /** Unpack the buffer into b. */
    position = 0;
    MPI_Unpack(buffer, bufsize, &position, b, 1, ltype, MPI_COMM_WORLD);
	
    for(i = 0; i < 100; i++) {
	for(j = 0; j < 100; j++) {
	    if (j > i && b[i][j] != 0) {
		errs++;
		if (verbose) fprintf(stderr, "b[%d][%d] = %d; should be %d\n",
				     i, j, b[i][j], 0);
	    }
	    else if (j <= i && b[i][j] != 1000*i + j) {
		errs++;
		if (verbose) fprintf(stderr, "b[%d][%d] = %d; should be %d\n",
				     i, j, b[i][j], 1000*i + j);
	    }
	}
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

int parse_args(int argc, char **argv)
{
    int ret;

    while ((ret = getopt(argc, argv, "v")) >= 0)
    {
	switch (ret) {
	    case 'v':
		verbose = 1;
		break;
	}
    }
    return 0;
}

}