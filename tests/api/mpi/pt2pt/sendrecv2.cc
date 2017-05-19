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
#include "mpitestconf.h"
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace sendrecv2 {
static int verbose = 0;

static int parse_args(int argc, char **argv);

int sendrecv2( int argc, char *argv[] )
{
    int i, j, errs = 0;
    int rank, size;
    MPI_Datatype newtype;
    char *buf = NULL;

    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size < 2) {
	if (verbose) fprintf(stderr, "comm size must be > 1\n");
	errs++;
	goto fn_exit;
    }

    buf = (char*)malloc(64 * 129);
    if (buf == NULL) {
	if (verbose) fprintf(stderr, "error allocating buffer\n");
	errs++;
	goto fn_exit;
    }

    for (i = 8; i < 64; i += 4) {
	MPI_Type_vector(i, 128, 129, MPI_CHAR, &newtype);

	MPI_Type_commit(&newtype);
	memset(buf, 0, 64*129);

	if (rank == 0) {
	    /** init buffer */
	    for (j=0; j < i; j++) {
		int k;
		for (k=0; k < 129; k++) {
		    buf[129*j + k] = (char) j;
		}
	    }

	    /** send */
	    MPI_Send(buf, 1, newtype, 1, i, MPI_COMM_WORLD);
	}
	else if (rank == 1) {
	    /** recv */
	    MPI_Recv(buf, 1, newtype, 0, i, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	    /** check buffer */
	    for (j=0; j < i; j++) {
		int k;
		for (k=0; k < 129; k++) {
		    if (k < 128 && buf[129*j + k] != (char) j) {
			if (verbose) fprintf(stderr,
					     "(i=%d, pos=%d) should be %d but is %d\n",
					     i, 129*j + k, j, (int) buf[129*j + k]);
			errs++;
		    }
		    else if (k == 128 && buf[129*j + k] != (char) 0) {
			if (verbose) fprintf(stderr,
					     "(i=%d, pos=%d) should be %d but is %d\n",
					     i, 129*j + k, 0, (int) buf[129*j + k]);
			errs++;
		    }
		}
	    }
	}

	MPI_Type_free(&newtype);
    }

    if (rank == 0) {
	int recv_errs = 0;

	MPI_Recv(&recv_errs, 1, MPI_INT, 1, 0, MPI_COMM_WORLD,
		 MPI_STATUS_IGNORE);
	if (recv_errs) {
	    if (verbose) fprintf(stderr, "%d errors reported from receiver\n",
				 recv_errs);
	    errs += recv_errs;
	}
    }
    else if (rank == 1) {
	MPI_Send(&errs, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
    }
	
 fn_exit:

    free(buf);
    /** print message and exit */
    if (errs) {
	if (rank == 0) fprintf(stderr, "Found %d errors\n", errs);
    }
    else {
	if (rank == 0) printf(" No Errors\n");
    }
    MPI_Finalize();
    return 0;
}

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