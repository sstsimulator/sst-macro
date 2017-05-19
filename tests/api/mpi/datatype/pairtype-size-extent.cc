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

namespace pairtype_size_extent {
static int verbose = 1;

static struct { MPI_Datatype atype, ptype; char name[32]; }
pairtypes[] =
    { {MPI_FLOAT, MPI_FLOAT_INT, "MPI_FLOAT_INT"},
      {MPI_DOUBLE, MPI_DOUBLE_INT, "MPI_DOUBLE_INT"},
      {MPI_LONG, MPI_LONG_INT, "MPI_LONG_INT"},
      {MPI_SHORT, MPI_SHORT_INT, "MPI_SHORT_INT"},
      {MPI_LONG_DOUBLE, MPI_LONG_DOUBLE_INT, "MPI_LONG_DOUBLE_INT"},
      {(MPI_Datatype) -1, (MPI_Datatype) -1, "end"}
    };

int parse_args(int argc, char **argv);

MPI_Aint pairtype_displacement(MPI_Datatype type, int *out_size_p);

MPI_Aint pairtype_displacement(MPI_Datatype type, int *out_size_p)
{
    MPI_Aint disp;

    /** Note that a portable test may not use a switch statement for 
       datatypes, as they are not required to be compile-time constants */
    if (type == MPI_FLOAT_INT) {
	struct { float a; int b; } foo;
	disp = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
	*out_size_p = sizeof(foo);
    }
    else if (type == MPI_DOUBLE_INT) {
	struct { double a; int b; } foo;
	disp = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
	*out_size_p = sizeof(foo);
    }
    else if (type == MPI_LONG_INT) {
	struct { long a; int b; } foo;
	disp = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
	*out_size_p = sizeof(foo);
    }
    else if (type == MPI_SHORT_INT) {
	struct { short a; int b; } foo;
	disp = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
	*out_size_p = sizeof(foo);
    }
    else if (type == MPI_LONG_DOUBLE_INT && type != MPI_DATATYPE_NULL) {
	struct { long double a; int b; } foo;
	disp = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
	*out_size_p = sizeof(foo);
    }
    else {
	disp = -1;
    }
    return disp;
}

int pairtype_size_extent(int argc, char *argv[])
{
    int errs = 0;

    int i;
    int blks[2] = {1, 1};
    MPI_Aint disps[2] = {0, 0};
    MPI_Datatype types[2] = {MPI_INT, MPI_INT};
    MPI_Datatype stype;
    
    MPI_Init(&argc, &argv);
    parse_args(argc, argv);

    for (i=0; pairtypes[i].atype != (MPI_Datatype) -1; i++) {
	int atype_size, ptype_size, stype_size, handbuilt_extent;
	MPI_Aint ptype_extent, stype_extent, dummy_lb;

	types[0] = pairtypes[i].atype;

	/** Check for undefined optional types, such as
	   LONG_DOUBLE_INT (if, for example, long double or
	   long long are not supported) */
	if (types[0] == MPI_DATATYPE_NULL) continue;

	MPI_Type_size(types[0], &atype_size);
	disps[1] = pairtype_displacement(pairtypes[i].ptype,
					 &handbuilt_extent);

	MPI_Type_create_struct(2, blks, disps, types, &stype);

	MPI_Type_size(stype, &stype_size);
	MPI_Type_size(pairtypes[i].ptype, &ptype_size);
	if (stype_size != ptype_size) {
	    errs++;

	    if (verbose) fprintf(stderr,
				 "size of %s (%d) does not match size of hand-built MPI struct (%d)\n",
				 pairtypes[i].name, ptype_size, stype_size);
	}

	MPI_Type_get_extent(stype, &dummy_lb, &stype_extent);
	MPI_Type_get_extent(pairtypes[i].ptype, &dummy_lb, &ptype_extent);
	if (stype_extent != ptype_extent || stype_extent != handbuilt_extent) {
	    errs++;

	    if (verbose) fprintf(stderr,
				 "extent of %s (%d) does not match extent of either hand-built MPI struct (%d) or equivalent C struct (%d)\n",
				 pairtypes[i].name, (int) ptype_extent,
				 (int) stype_extent,
				 handbuilt_extent);
	}
	MPI_Type_free( &stype );
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
    /** We use a simple test because getopt isn't universally available */
    if (argc > 1 && strcmp(argv[1], "-v") == 0)
	verbose = 1;
    if (argc > 1 && strcmp(argv[1], "-nov") == 0)
	verbose = 0;
    return 0;
}

}