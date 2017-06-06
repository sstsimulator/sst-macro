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
#include <sstmac/replacements/mpi.h>
#include "mpitest.h"

namespace typecommit {

void foo(void *sendbuf, MPI_Datatype sendtype, void *recvbuf, 
	 MPI_Datatype recvtype);
void foo(void *sendbuf, MPI_Datatype sendtype, void *recvbuf, 
	 MPI_Datatype recvtype)
{
    int blocks[2];
    MPI_Aint struct_displs[2];
    MPI_Datatype types[2], tmp_type;

    blocks[0] = 256;
    MPI_Get_address( sendbuf, &struct_displs[0] );
    types[0] = sendtype;
    blocks[1] = 256;
    MPI_Get_address( recvbuf, &struct_displs[1] );
    types[1] = MPI_BYTE;

    MPI_Type_create_struct(2, blocks, struct_displs, types, &tmp_type);
    MPI_Type_commit(&tmp_type);
    MPI_Type_free(&tmp_type);
}

int typecommit(int argc, char **argv)
{
    int errs = 0;

    MTest_Init(&argc, &argv);

    foo((void*) 0x1, MPI_FLOAT_INT, (void*) 0x2, MPI_BYTE);
    foo((void*) 0x1, MPI_DOUBLE_INT, (void*) 0x2, MPI_BYTE);
    foo((void*) 0x1, MPI_LONG_INT, (void*) 0x2, MPI_BYTE);
    foo((void*) 0x1, MPI_SHORT_INT, (void*) 0x2, MPI_BYTE);
    foo((void*) 0x1, MPI_2INT, (void*) 0x2, MPI_BYTE);
#ifdef HAVE_LONG_DOUBLE
    /** Optional type may be NULL */
    if (MPI_LONG_DOUBLE_INT != MPI_DATATYPE_NULL) {
	foo((void*) 0x1, MPI_LONG_DOUBLE_INT, (void*) 0x2, MPI_BYTE);
    }
#endif

    MTest_Finalize(errs);
    MPI_Finalize();

    return 0;
}

}