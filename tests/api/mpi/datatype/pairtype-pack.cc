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

namespace pairtype_pack{
static int verbose = 0;

int short_int_pack_test(void);

/** helper functions */
int parse_args(int argc, char **argv);
static int pack_and_unpack(char *typebuf,
			   int count,
			   MPI_Datatype datatype,
			   int typebufsz);

int pairtype_pack(int argc, char *argv[])
{
    int err, errs = 0;

    MPI_Init(&argc, &argv); /** MPI-1.2 doesn't allow for MPI_Init(0,0) */
    //parse_args(argc, argv);

    /** To improve reporting of problems about operations, we
       change the error handler to errors return */
    MPI_Comm_set_errhandler( MPI_COMM_WORLD, MPI_ERRORS_RETURN );

    err = short_int_pack_test();
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

int short_int_pack_test(void)
{
    int i, err, errs = 0;

    struct shortint { short a; int b; } sibuf[16];

    for (i=0; i < 16; i++) {
	sibuf[i].a = (short) (i * 2);
	sibuf[i].b = i * 2 + 1;
    }

    std::cout << "offset of a: "<< offsetof(shortint, a) << ", offset of b: " << offsetof(shortint, b) << ", size of whole thing: " << sizeof(sibuf) << "\n";
    std::cout << "size of short: " << sizeof(short) << "\n";

    err = pack_and_unpack((char *) sibuf, 16, MPI_SHORT_INT, sizeof(sibuf));
    if (err != 0) {
	if (verbose) {
	    fprintf(stderr,
		    "error packing/unpacking in short_int_pack_test()\n");
	}
	errs += err;
    }

    for (i=0; i < 16; i++) {
	if (sibuf[i].a != (short) (i * 2)) {
	    err++;
	    if (verbose) {
		fprintf(stderr,
			"buf[%d] has invalid short (%d); should be %d\n",
			i, (int) sibuf[i].a, i * 2);
	    }
	}
	if (sibuf[i].b != i * 2 + 1) {
	    err++;
	    if (verbose) {
		fprintf(stderr,
			"buf[%d] has invalid int (%d); should be %d\n",
			i, (int) sibuf[i].b, i * 2 + 1);
	    }
	}
    }

    return errs;
}

/** pack_and_unpack()
 *
 * Perform packing and unpacking of a buffer for the purposes of checking
 * to see if we are processing a type correctly.  Zeros the buffer between
 * these two operations, so the data described by the type should be in
 * place upon return but all other regions of the buffer should be zero.
 *
 * Parameters:
 * typebuf - pointer to buffer described by datatype and count that
 *           will be packed and then unpacked into
 * count, datatype - description of typebuf
 * typebufsz - size of typebuf; used specifically to zero the buffer
 *             between the pack and unpack steps
 *
 */
static int pack_and_unpack(char *typebuf,
			   int count,
			   MPI_Datatype datatype,
			   int typebufsz)
{
    char *packbuf;
    int err, errs = 0, pack_size, type_size, position;

    err = MPI_Type_size(datatype, &type_size);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr,
		    "error in MPI_Type_size call; aborting after %d errors\n",
		    errs);
	}
	return errs;
    }

    type_size *= count;

    err = MPI_Pack_size(count, datatype, MPI_COMM_SELF, &pack_size);
    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr,
		    "error in MPI_Pack_size call; aborting after %d errors\n",
		    errs);
	}
	return errs;
    }
    packbuf = (char *) malloc(pack_size);
    if (packbuf == NULL) {
	errs++;
	if (verbose) {
	    fprintf(stderr,
		    "error in malloc call; aborting after %d errors\n",
		    errs);
	}
	return errs;
    }

    position = 0;
    err = MPI_Pack(typebuf,
		   count,
		   datatype,
		   packbuf,
		   type_size,
		   &position,
		   MPI_COMM_SELF);

    if (position != type_size) {
	errs++;
	if (verbose) fprintf(stderr, "position = %d; should be %d (pack)\n",
			     position, type_size);
    }

    memset(typebuf, 0, typebufsz);
    position = 0;
    err = MPI_Unpack(packbuf,
		     type_size,
		     &position,
		     typebuf,
		     count,
		     datatype,
		     MPI_COMM_SELF);

    if (err != MPI_SUCCESS) {
	errs++;
	if (verbose) {
	    fprintf(stderr,
		    "error in MPI_Unpack call; aborting after %d errors\n",
		    errs);
	}
	return errs;
    }
    free(packbuf);

    if (position != type_size) {
	errs++;
	if (verbose) fprintf(stderr, "position = %d; should be %d (unpack)\n",
			     position, type_size);
    }

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