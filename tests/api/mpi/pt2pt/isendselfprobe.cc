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

namespace isendselfprobe {
int isendselfprobe( int argc, char * argv[] )
{
    int rank;
    int sendMsg = 123;
    int recvMsg = 0;
    int flag = 0;
    int count;
    MPI_Status status;
    MPI_Request request;
    int errs = 0;

    MTest_Init( 0, 0 );

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if(rank == 0)
    {
	MPI_Isend( &sendMsg, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &request );
	while(!flag)
	{
	    MPI_Iprobe( 0, 0, MPI_COMM_WORLD, &flag, &status );
	}
	MPI_Get_count( &status, MPI_INT, &count );
	if(count != 1)
	{
	    errs++;
	}
	MPI_Recv( &recvMsg, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status );
	if(recvMsg != 123)
	{
	    errs++;
	}
	MPI_Wait( &request, &status );
    }
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}