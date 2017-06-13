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
#include <stdlib.h>
#include <stdio.h>
#include "mpitest.h"

namespace keyval_double_free {

/** tests multiple invocations of Keyval_free on the same keyval */

int delete_fn(MPI_Comm comm, int keyval, void *attr, void *extra);
int delete_fn(MPI_Comm comm, int keyval, void *attr, void *extra) {
    MPI_Keyval_free(&keyval);
    return MPI_SUCCESS;
}

int keyval_double_free (int argc, char **argv)
{
    MPI_Comm duped;
    int keyval = MPI_KEYVAL_INVALID;
    int keyval_copy = MPI_KEYVAL_INVALID;
    int errs=0;

    MTest_Init( &argc, &argv );
    MPI_Comm_dup(MPI_COMM_SELF, &duped);

    MPI_Keyval_create(MPI_NULL_COPY_FN, delete_fn,  &keyval, NULL);
    keyval_copy = keyval;

    MPI_Attr_put(MPI_COMM_SELF, keyval, NULL);
    MPI_Attr_put(duped, keyval, NULL);

    MPI_Comm_free(&duped);         /** first MPI_Keyval_free */
    MPI_Keyval_free(&keyval);      /** second MPI_Keyval_free */
    MPI_Keyval_free(&keyval_copy); /** third MPI_Keyval_free */
    MTest_Finalize( errs );
    MPI_Finalize();                /** fourth MPI_Keyval_free */
    return 0;
}

}