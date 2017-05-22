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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace transpose_pack {
static int verbose = 0;

int transpose_pack(int argc, char *argv[]);
int parse_args(int argc, char **argv);

int transpose_pack(int argc, char *argv[])
{
    /** Variable declarations */
    int a[100][100], b[100][100];
    MPI_Datatype row, xpose;
    MPI_Aint sizeofint;
	
    int err, errs = 0;
    int bufsize, position = 0;
    void *buffer;
  
    int i, j;
  
    /** Initialize a to some known values. */
    for(i = 0; i < 100; i++) {
	for(j = 0; j < 100; j++) {
	    a[i][j] = i*1000+j;
	    b[i][j] = -1;
	}
    }
  
    /** Initialize MPI */
    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    MPI_Type_extent(MPI_INT, &sizeofint);
	
    /** Create datatypes. */
    MPI_Type_vector(100, 1, 100, MPI_INT, &row);
    MPI_Type_hvector(100, 1, sizeofint, row, &xpose);
    MPI_Type_commit(&xpose);
	
    /** Pack it. */
    MPI_Pack_size(1, xpose, MPI_COMM_WORLD, &bufsize);
    buffer = (char *) malloc((unsigned) bufsize);

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    err = MPI_Pack(a,
		   1,
		   xpose,
		   buffer,
		   bufsize,
		   &position,
		   MPI_COMM_WORLD);
	
    /** Unpack the buffer into b. */
    position = 0;
    err = MPI_Unpack(buffer,
		     bufsize,
		     &position,
		     b,
		     100*100,
		     MPI_INT,
		     MPI_COMM_WORLD);

    for (i = 0; i < 100; i++) {
	for (j = 0; j < 100; j++) {
	    if(b[i][j] != a[j][i]) {
		errs++;
		if (verbose) fprintf(stderr, "b[%d][%d] = %d, should be %d\n",
				     i, j, b[i][j], a[j][i]);
	    }
	}
    }

    MPI_Type_free(&xpose);
    MPI_Type_free(&row);
    
    /** print message and exit */
    if (errs) {
	fprintf(stderr, "Found %d errors\n", errs);
    }
    else {
	printf(" No Errors\n");
    }
    MPI_Finalize();
    return 0;
}


int parse_args(int argc, char **argv)
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