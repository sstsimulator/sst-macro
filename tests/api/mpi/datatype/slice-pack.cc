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
#include <math.h>
#include <stdlib.h>
#include "mpitest.h"
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace slice_pack {

static int verbose = 0;
int a[100][100][100], e[9][9][9];

int slice_pack(int argc, char *argv[]);

/** helper functions */
static int parse_args(int argc, char **argv);

int slice_pack(int argc, char *argv[])
{
    /** Variable declarations */
    MPI_Datatype oneslice, twoslice, threeslice;
    int errs = 0;
    MPI_Aint sizeofint;
	
    int bufsize, position;
    void *buffer;
	
    int i, j, k;
	
    /** Initialize a to some known values. */
    for (i = 0; i < 100; i++) {
	for (j = 0; j < 100; j++) {
	    for (k = 0; k < 100; k++) {
		a[i][j][k] = i*1000000+j*1000+k;
	    }
	}
    }
	
    /** Initialize MPI */
    MPI_Init(&argc, &argv);
    MPI_Type_extent(MPI_INT, &sizeofint);
  
    parse_args(argc, argv);

    /** Create data types. */
    /** NOTE: This differs from the way that it's done on the sheet. */
    /** On the sheet, the slice is a[0, 2, 4, ..., 16][2-10][1-9]. */
    /** Below, the slice is a[0-8][2-10][1, 3, 5, ..., 17]. */
    MPI_Type_vector(9, 1, 2, MPI_INT, &oneslice);
    MPI_Type_hvector(9, 1, 100*sizeofint, oneslice, &twoslice);
    MPI_Type_hvector(9, 1, 100*100*sizeofint, twoslice, &threeslice);
	
    MPI_Type_commit(&threeslice);
	
    /** Pack it into a buffer. */
    position = 0;
    MPI_Pack_size(1, threeslice, MPI_COMM_WORLD, &bufsize);
    buffer = (void *) malloc((unsigned) bufsize);

    /** -1 to indices on sheet to compensate for Fortran --> C */
    MPI_Pack(&(a[0][2][1]),
	     1, threeslice,
	     buffer,
	     bufsize,
	     &position,
	     MPI_COMM_WORLD);

    /** Unpack the buffer into e. */
    position = 0;
    MPI_Unpack(buffer,
	       bufsize,
	       &position,
	       e, 9*9*9,
	       MPI_INT,
	       MPI_COMM_WORLD);
	
    /** Display errors, if any. */
    for (i = 0; i < 9; i++) {
	for (j = 0; j < 9; j++) {
	    for (k = 0; k < 9; k++) {
	       /** The truncation in integer division makes this safe. */
		if (e[i][j][k] != a[i][j+2][k*2+1]) {
		    errs++;
		    if (verbose) {
			printf("Error in location %d x %d x %d: %d, should be %d.\n",
			       i, j, k, e[i][j][k], a[i][j+2][k*2+1]);
		    }
		}
	    }
	}
    } 
  
    /** Release memory. */
    free(buffer);

    if (errs) {
	fprintf(stderr, "Found %d errors\n", errs);
    }
    else {
	printf(" No Errors\n");
    }

    MPI_Type_free(&oneslice);
    MPI_Type_free(&twoslice);
    MPI_Type_free(&threeslice);

    MPI_Finalize();
    return 0;
}

/** parse_args()
 */
static int parse_args(int argc, char **argv)
{
    /**
    int ret;

    while ((ret = getopt(argc, argv, "v")) >= 0)
    {
	switch (ret) {
	    case 'v':
		verbose = 1;
		break;
	}
    }
    */
    if (argc > 1 && strcmp(argv[1], "-v") == 0)
	verbose = 1;
    return 0;
}

}