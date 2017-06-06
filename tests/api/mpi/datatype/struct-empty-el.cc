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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sstmac/replacements/mpi.h>

namespace struct_empty_el {
static int verbose = 1;

int struct_empty_el(int argc, char *argv[]);
int parse_args(int argc, char **argv);
int single_struct_test(void);

struct test_struct_1 {
    int a,b,c,d;
};

int struct_empty_el(int argc, char *argv[])
{
    int err, errs = 0;

    /** Initialize MPI */
    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    err = single_struct_test();
    if (verbose && err) fprintf(stderr, "error in single_struct_test\n");
    errs += err;

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

int single_struct_test(void)
{
    int err, errs = 0;
    int count, elements;
    int sendbuf[6] = { 1, 2, 3, 4, 5, 6 };
    struct test_struct_1 ts1[2];
    MPI_Datatype mystruct;
    MPI_Request request;
    MPI_Status status;

    /** note: first element of struct has zero blklen and should be dropped */
    MPI_Aint disps[3]     = { 2*sizeof(float), 0,       2*sizeof(int) };
    int blks[3]           = { 0,               1,       2 };
    MPI_Datatype types[3] = { MPI_FLOAT,       MPI_INT, MPI_INT };

    ts1[0].a = -1;
    ts1[0].b = -1;
    ts1[0].c = -1;
    ts1[0].d = -1;

    ts1[1].a = -1;
    ts1[1].b = -1;
    ts1[1].c = -1;
    ts1[1].d = -1;

    err = MPI_Type_struct(3, blks, disps, types, &mystruct);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "MPI_Type_struct returned error\n");
	}
    }

    MPI_Type_commit(&mystruct);

    err = MPI_Irecv(ts1, 2, mystruct, 0, 0, MPI_COMM_SELF, &request);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "MPI_Irecv returned error\n");
	}
    }

    err = MPI_Send(sendbuf, 6, MPI_INT, 0, 0, MPI_COMM_SELF);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "MPI_Send returned error\n");
	}
    }

    err = MPI_Wait(&request, &status);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "MPI_Wait returned error\n");
	}
    }

    /** verify data */
    if (ts1[0].a != 1) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[0].a = %d; should be %d\n", ts1[0].a, 1);
	}
    }
    if (ts1[0].b != -1) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[0].b = %d; should be %d\n", ts1[0].b, -1);
	}
    }
    if (ts1[0].c != 2) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[0].c = %d; should be %d\n", ts1[0].c, 2);
	}
    }
    if (ts1[0].d != 3) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[0].d = %d; should be %d\n", ts1[0].d, 3);
	}
    }
    if (ts1[1].a != 4) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[1].a = %d; should be %d\n", ts1[1].a, 4);
	}
    }
    if (ts1[1].b != -1) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[1].b = %d; should be %d\n", ts1[1].b, -1);
	}
    }
    if (ts1[1].c != 5) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[1].c = %d; should be %d\n", ts1[1].c, 5);
	}
    }
    if (ts1[1].d != 6) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "ts1[1].d = %d; should be %d\n", ts1[1].d, 6);
	}
    }

    /** verify count and elements */
    err = MPI_Get_count(&status, mystruct, &count);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "MPI_Get_count returned error\n");
	}
    }
    if (count != 2) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "count = %d; should be 2\n", count);
	}
    }

    err = MPI_Get_elements(&status, mystruct, &elements);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "MPI_Get_elements returned error\n");
	}
    }
    if (elements != 6) {
	errs++;
	if (verbose) {
	    fprintf(stderr, "elements = %d; should be 6\n", elements);
	}
    }

    MPI_Type_free(&mystruct);

    return errs;
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