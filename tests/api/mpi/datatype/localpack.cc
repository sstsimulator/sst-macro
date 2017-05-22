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

namespace localpack {
static int verbose = 1;

#define BUF_SIZE 4096

int localpack(int argc, char *argv[]);
int parse_args(int argc, char **argv);

int localpack(int argc, char *argv[])
{
    int errs = 0;
    char buffer[BUF_SIZE];
    int n, size;
    double a,b;
    int pos;

    /** Initialize MPI */
    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    pos	= 0;
    n	= 10;
    a	= 1.1;
    b	= 2.2;

    MPI_Pack(&n, 1, MPI_INT, buffer, BUF_SIZE, &pos, MPI_COMM_WORLD);
    MPI_Pack(&a, 1, MPI_DOUBLE, buffer, BUF_SIZE, &pos, MPI_COMM_WORLD);
    MPI_Pack(&b, 1, MPI_DOUBLE, buffer, BUF_SIZE, &pos, MPI_COMM_WORLD);

    size = pos;
    pos  = 0;
    n    = 0;
    a    = 0;
    b    = 0;

    MPI_Unpack(buffer, size, &pos, &n, 1, MPI_INT, MPI_COMM_WORLD);
    MPI_Unpack(buffer, size, &pos, &a, 1, MPI_DOUBLE, MPI_COMM_WORLD);
    MPI_Unpack(buffer, size, &pos, &b, 1, MPI_DOUBLE, MPI_COMM_WORLD);
    /** Check results */
    if (n != 10) { 
	errs++;
	if (verbose) fprintf(stderr, "Wrong value for n; got %d expected %d\n", n, 10 );
    }
    if (a != 1.1) { 
	errs++;
	if (verbose) fprintf(stderr, "Wrong value for a; got %f expected %f\n", a, 1.1 );
    }
    if (b != 2.2) { 
	errs++;
	if (verbose) fprintf(stderr, "Wrong value for b; got %f expected %f\n", b, 2.2 );
    }

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