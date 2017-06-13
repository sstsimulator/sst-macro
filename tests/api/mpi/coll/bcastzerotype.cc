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
#include <assert.h>

#include <sstmac/replacements/mpi.h>

namespace bcastzerotype {
/** test broadcast behavior with non-zero counts but zero-sized types */

int bcastzerotype(int argc, char *argv[])
{
    int i, type_size;
    MPI_Datatype type = MPI_DATATYPE_NULL;
    char *buf = NULL;
    int wrank, wsize;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    MPI_Comm_size(MPI_COMM_WORLD, &wsize);

    /** a random non-zero sized buffer */
#define NELEM (10)
    buf =(char*) malloc(NELEM*sizeof(int));
    assert(buf);

    for (i = 0; i < NELEM; i++) {
        buf[i] = wrank * NELEM + i;
    }

    /** create a zero-size type */
    MPI_Type_contiguous(0, MPI_INT, &type);
    MPI_Type_commit(&type);
    MPI_Type_size(type, &type_size);
    assert(type_size == 0);

    /** do the broadcast, which will break on some MPI implementations */
    MPI_Bcast(buf, NELEM, type, 0, MPI_COMM_WORLD);

    /** check that the buffer remains unmolested */
    for (i = 0; i < NELEM; i++) {
        assert(buf[i] == wrank * NELEM + i);
    }

    MPI_Type_free(&type);
    MPI_Finalize();

    if (wrank == 0) {
        printf(" No Errors\n");
    }

    return 0;
}

}