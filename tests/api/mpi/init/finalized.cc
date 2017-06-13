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

namespace finalized {
/** FIXME: This test program assumes that MPI_Error_string will work even
   if MPI is not initialized.  That is not guaranteed.  */

/** Normally, when checking for error returns from MPI calls, you must ensure 
   that the error handler on the relevant object (communicator, file, or
   window) has been set to MPI_ERRORS_RETURN.  The tests in this 
   program are a special case, as either a failure or an abort will
   indicate a problem */

int finalized( int argc, char *argv[] )
{
    int error;
    int flag;
    char err_string[1024];
    int length = 1024;
    int rank;

    flag = 0;
    error = MPI_Finalized(&flag);
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Finalized failed: %s\n", err_string);
	fflush(stdout);
	return error;
    }
    if (flag)
    {
	printf("MPI_Finalized returned true before MPI_Init.\n");
	return -1;
    }

    error = MPI_Init(&argc, &argv);
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Init failed: %s\n", err_string);
	fflush(stdout);
	return error;
    }

    error = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Comm_rank failed: %s\n", err_string);
	fflush(stdout);
	MPI_Abort(MPI_COMM_WORLD, error);
	return error;
    }

    flag = 0;
    error = MPI_Finalized(&flag);
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Finalized failed: %s\n", err_string);
	fflush(stdout);
	MPI_Abort(MPI_COMM_WORLD, error);
	return error;
    }
    if (flag)
    {
	printf("MPI_Finalized returned true before MPI_Finalize.\n");
	fflush(stdout);
	MPI_Abort(MPI_COMM_WORLD, error);
	return -1;
    }

    error = MPI_Barrier(MPI_COMM_WORLD);
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Barrier failed: %s\n", err_string);
	fflush(stdout);
	MPI_Abort(MPI_COMM_WORLD, error);
	return error;
    }

    error = MPI_Finalize();
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Finalize failed: %s\n", err_string);
	fflush(stdout);
	return error;
    }

    flag = 0;
    error = MPI_Finalized(&flag);
    if (error != MPI_SUCCESS)
    {
	MPI_Error_string(error, err_string, &length);
	printf("MPI_Finalized failed: %s\n", err_string);
	fflush(stdout);
	return error;
    }
    if (!flag)
    {
	printf("MPI_Finalized returned false after MPI_Finalize.\n");
	return -1;
    }
    if (rank == 0)
    {
	printf(" No Errors\n");
    }
    return 0;  
}

}