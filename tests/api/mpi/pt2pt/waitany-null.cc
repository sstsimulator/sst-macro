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
#include <stdlib.h>
#include "mpitestconf.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <sstmac/replacements/mpi.h>

namespace waitany_null {
static int verbose = 0;

int waitany_null(int argc, char *argv[]);
int parse_args(int argc, char **argv);

int waitany_null(int argc, char *argv[])
{
    int i, err, errs = 0, rank, toterrs;

    int         index;
    MPI_Request requests[10];
    MPI_Status  statuses[10];

    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    for (i=0; i < 10; i++) {
	requests[i] = MPI_REQUEST_NULL;
    }

    /** begin testing */
    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    err = MPI_Waitany(10, requests, &index, statuses);

    if (err != MPI_SUCCESS) {
	errs++;
	fprintf(stderr, "MPI_Waitany did not return MPI_SUCCESS\n");
    }

    if (index != MPI_UNDEFINED) {
	errs++;
	fprintf(stderr, "MPI_Waitany did not set index to MPI_UNDEFINED\n");
    }

    /** end testing */
    
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL );
    MPI_Comm_rank( MPI_COMM_WORLD, & rank );
    MPI_Allreduce( &errs, &toterrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD );
    if (rank == 0) {
	if (toterrs) {
	    fprintf(stderr, " Found %d errors\n", toterrs);
	}
	else {
	    printf(" No Errors\n");
	}
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