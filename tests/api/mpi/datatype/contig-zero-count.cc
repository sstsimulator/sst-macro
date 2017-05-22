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
namespace contig_zero_count {

static int verbose = 0;

/** tests */
int contig_test(void);

/** helper functions */
int parse_args(int argc, char **argv);

int contig_zero_count(int argc, char **argv)
{
    int err, errs = 0;

    MPI_Init(&argc, &argv); /** MPI-1.2 doesn't allow for MPI_Init(0,0) */
    parse_args(argc, argv);

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    /** perform some tests */
    err = contig_test();
    if (err && verbose) fprintf(stderr, "%d errors in contig test.\n",
				err);
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

/** contig_test()
 *
 * Tests behavior with a zero-count contig.
 *
 * Returns the number of errors encountered.
 */
int contig_test(void)
{
    int err, errs = 0;

    int count = 0;
    MPI_Datatype newtype;

    int size;
    MPI_Aint extent;

    err = MPI_Type_contiguous(count,
			     MPI_INT,
			     &newtype);
    if (err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr,
		    "error creating type in contig_test()\n");
	}
	errs++;
    }

    err = MPI_Type_size(newtype, &size);
    if (err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr,
		    "error obtaining type size in contig_test()\n");
	}
	errs++;
    }
    
    if (size != 0) {
	if (verbose) {
	    fprintf(stderr,
		    "error: size != 0 in contig_test()\n");
	}
	errs++;
    }    

    err = MPI_Type_extent(newtype, &extent);
    if (err != MPI_SUCCESS) {
	if (verbose) {
	    fprintf(stderr,
		    "error obtaining type extent in contig_test()\n");
	}
	errs++;
    }
    
    if (extent != 0) {
	if (verbose) {
	    fprintf(stderr,
		    "error: extent != 0 in contig_test()\n");
	}
	errs++;
    }    

    MPI_Type_free( &newtype );

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