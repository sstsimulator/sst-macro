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
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

namespace simple_size_extent {

static int verbose = 0;

int parse_args(int argc, char **argv);

int simple_size_extent(int argc, char **argv)
{
    int mpi_err, errs = 0, size;
    MPI_Aint lb, ub, extent;
    MPI_Datatype type;

    struct { float a; int b; } foo;

    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    type = MPI_INT;
    mpi_err = MPI_Type_size(type, &size);
    if (mpi_err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_size of MPI_INT failed.\n");
	}
	errs++;
    }
    if (size != sizeof(int)) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_size of MPI_INT incorrect size (%d); should be %d.\n",
		    size, (int) sizeof(int));
	}
	errs++;
    }

    mpi_err = MPI_Type_get_extent(type, &lb, &extent);
    if (mpi_err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_get_extent of MPI_INT failed.\n");
	}
	errs++;
    }
    if (extent != sizeof(int)) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_get_extent of MPI_INT returned incorrect extent (%d); should be %d.\n",
		    (int) extent, (int) sizeof(int));
	}
	errs++;
    }
    if (lb != 0) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_get_extent of MPI_INT returned incorrect lb (%d); should be 0.\n",
		    (int) lb);
	}
	errs++;
    }
    mpi_err = MPI_Type_ub(type, &ub);
    if (mpi_err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_ub of MPI_INT failed.\n");
	}
	errs++;
    }
    if (ub != extent - lb) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_ub of MPI_INT returned incorrect ub (%d); should be %d.\n",
		    (int) ub, (int) (extent - lb));
	}
	errs++;
    }

    type = MPI_FLOAT_INT;
    mpi_err = MPI_Type_size(type, &size);
    if (mpi_err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_size of MPI_FLOAT_INT failed.\n");
	}
	errs++;
    }
    if (size != sizeof(float) + sizeof(int)) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_size of MPI_FLOAT_INT returned incorrect size (%d); should be %d.\n",
		    size, (int) (sizeof(float) + sizeof(int)));
	}
	errs++;
    }

    mpi_err = MPI_Type_get_extent(type, &lb, &extent);
    if (mpi_err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_get_extent of MPI_FLOAT_INT failed.\n");
	}
	errs++;
    }
    if (extent != sizeof(foo)) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_get_extent of MPI_FLOAT_INT returned incorrect extent (%d); should be %d.\n",
		    (int) extent, (int) sizeof(foo));
	}
	errs++;
    }
    if (lb != 0) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_get_extent of MPI_FLOAT_INT returned incorrect lb (%d); should be 0.\n",
		    (int) lb);
	}
	errs++;
    }
    mpi_err = MPI_Type_ub(type, &ub);
    if (mpi_err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_ub of MPI_FLOAT_INT failed.\n");
	}
	errs++;
    }
    if (ub != extent - lb) {
	if (verbose) {
	    fprintf(stderr, "MPI_Type_ub of MPI_FLOAT_INT returned incorrect ub (%d); should be %d.\n",
		    (int) ub, (int) (extent - lb));
	}
	errs++;
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