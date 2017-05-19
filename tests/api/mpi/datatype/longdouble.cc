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

#include <stdlib.h>
#include <stdio.h>
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace longdouble {
/** Some MPI implementations should not support MPI_LONG_DOUBLE because it has
 * different representations/sizes among several concurrently supported
 * compilers.  For example, a 16-byte GCC implementation and an 8-byte Cray
 * compiler implementation.
 *
 * This test ensures that simplistic build logic/configuration did not result in
 * a defined, yet incorrectly sized, MPI predefined datatype for long double and
 * long double _Complex.  See tt#1671 for more info.
 *
 * Based on a test suggested by Jim Hoekstra @ Iowa State University. */

int longdouble(int argc, char *argv[])
{
    int rank, size, i, type_size;
    int errs = 0;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (rank == 0) {
#ifdef HAVE_LONG_DOUBLE
        if (MPI_LONG_DOUBLE != MPI_DATATYPE_NULL) {
            MPI_Type_size(MPI_LONG_DOUBLE, &type_size);
            if (type_size != sizeof(long double)) {
                printf("type_size != sizeof(long double) : (%zd != %zd)\n",
                       type_size, sizeof(long double));
                ++errs;
            }
        }
#endif
#if defined(HAVE_LONG_DOUBLE__COMPLEX) && defined(USE_LONG_DOUBLE_COMPLEX)
        if (MPI_C_LONG_DOUBLE_COMPLEX != MPI_DATATYPE_NULL) {
            MPI_Type_size(MPI_C_LONG_DOUBLE_COMPLEX, &type_size);
            if (type_size != sizeof(long double _Complex)) {
                printf("type_size != sizeof(long double _Complex) : (%zd != %zd)\n",
                       type_size, sizeof(long double _Complex));
                ++errs;
            }
        }
#endif
        if (errs) {
            printf("found %d errors\n", errs);
        }
        else {
            printf(" No Errors\n");
        }
    }

    MPI_Finalize();
    return 0;
}

}