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

namespace zero_blklen_vector {
int zero_blklen_vector(int argc, char* argv[])
{
	int iam, np;
	int m = 2, n = 0, lda = 1;
	double A[2];
	MPI_Comm comm = MPI_COMM_WORLD;
	MPI_Datatype type = MPI_DOUBLE, vtype;

	MPI_Init(&argc,&argv);
	MPI_Comm_size(comm, &np);
	MPI_Comm_rank(comm, &iam);
	if (np < 2) {
		printf( "Should be at least 2 processes for the test\n");
        } else {
		MPI_Type_vector(n, m, lda, type, &vtype);
		MPI_Type_commit(&vtype);
        A[0] = -1.0-0.1*iam;
		A[1] = 0.5+0.1*iam;
        printf("In process %i of %i before Bcast: A = %f,%f\n",
		       iam, np, A[0], A[1] );
		MPI_Bcast(A, 1, vtype, 0, comm);
        printf("In process %i of %i after Bcast: A = %f,%f\n",
		       iam, np, A[0], A[1]);
		MPI_Type_free(&vtype);
    }

	printf("No Errors \n");

	MPI_Finalize();
    return 0;
}

}