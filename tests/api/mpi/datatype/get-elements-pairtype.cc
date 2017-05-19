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
#include <string.h>

namespace get_elements_pairtype {
static int verbose = 1;

/** tests */
int double_int_test(void);

/** helper functions */
int parse_args(int argc, char **argv);

int get_elements_pairtype(int argc, char **argv)
{
    int err, errs = 0;

    MPI_Init(&argc, &argv); /** MPI-1.2 doesn't allow for MPI_Init(0,0) */
    parse_args(argc, argv);

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    /** perform some tests */
    err = double_int_test();
    if (err && verbose) fprintf(stderr, "%d errors in double_int test.\n",
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

/** send a { double, int, double} tuple and receive as a pair of
 * MPI_DOUBLE_INTs. this should (a) be valid, and (b) result in an
 * element count of 3.
 */
int double_int_test(void)
{
    int err, errs = 0, count;

    struct { double a; int b; double c; } foo;
    struct { double a; int b; double c; int d; } bar;

    int blks[3] = { 1, 1, 1 };
    MPI_Aint disps[3] = { 0, 0, 0 };
    MPI_Datatype types[3] = { MPI_DOUBLE, MPI_INT, MPI_DOUBLE };
    MPI_Datatype stype;

    MPI_Status recvstatus;

    /** fill in disps[1..2] with appropriate offset */
    disps[1] = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
    disps[2] = (MPI_Aint) ((char *) &foo.c - (char *) &foo.a);
   
    MPI_Type_create_struct(3, blks, disps, types, &stype);
    MPI_Type_commit(&stype);

    err = MPI_Sendrecv(&foo, 1, stype, 0, 0,
		       &bar, 2, MPI_DOUBLE_INT, 0, 0,
		       MPI_COMM_SELF, &recvstatus);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) fprintf(stderr, "MPI_Sendrecv returned error (%d)\n",
			     err);
	return errs;
    }

    err = MPI_Get_elements(&recvstatus, MPI_DOUBLE_INT, &count);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) fprintf(stderr, "MPI_Get_elements returned error (%d)\n",
			     err);
    }

    if (count != 3) {
	errs++;
	if (verbose) fprintf(stderr,
			     "MPI_Get_elements returned count of %d, should be 3\n",
			     count);
    }

    MPI_Type_free( &stype );

    return errs;
}

int parse_args(int argc, char **argv)
{
    if (argc > 1 && strcmp(argv[1], "-v") == 0)
	verbose = 1;
    return 0;
}

}