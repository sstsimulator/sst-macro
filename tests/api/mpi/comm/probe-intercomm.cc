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
#include "mpitest.h"

namespace probe_intercomm{
/**
static char MTEST_Descrip[] = "Test MPI_Probe() for an intercomm";
*/
#define MAX_DATA_LEN 100

int probe_intercomm( int argc, char *argv[] )
{
    int errs = 0, recvlen, isLeft;
    MPI_Status status;
    int rank, size;
    MPI_Comm  intercomm;
    char buf[MAX_DATA_LEN];
    const char *test_str = "test";

    MTest_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );

    if (size < 2) {
	fprintf( stderr, "This test requires at least two processes." );
	MPI_Abort( MPI_COMM_WORLD, 1 );
    }

    while (MTestGetIntercomm( &intercomm, &isLeft, 2 )) {
        if (intercomm == MPI_COMM_NULL) continue;

        MPI_Comm_rank(intercomm, &rank);

        /** 0 ranks on each side communicate, everyone else does nothing */
        if(rank == 0) {
            if (isLeft) {
                recvlen = -1;
                MPI_Probe(0, 0, intercomm, &status);
                MPI_Get_count(&status, MPI_CHAR, &recvlen);
                if (recvlen != (strlen(test_str) + 1)) {
                    printf(" Error: recvlen (%d) != strlen(\"%s\")+1 (%d)\n", recvlen, test_str, (int)strlen(test_str) + 1);
                    ++errs;
                }
                buf[0] = '\0';
                MPI_Recv(buf, recvlen, MPI_CHAR, 0, 0, intercomm, &status);
                if (strcmp(test_str,buf)) {
                    printf(" Error: strcmp(test_str,buf)!=0\n");
                    ++errs;
                }
            }
            else {
                strncpy(buf, test_str, 5);
                MPI_Send(buf, strlen(buf)+1, MPI_CHAR, 0, 0, intercomm);
            }
        }
        MTestFreeComm(&intercomm);
    }

    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
}

}