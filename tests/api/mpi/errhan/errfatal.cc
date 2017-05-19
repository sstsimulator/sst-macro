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

namespace errfatal {

/** FIXME: This behavior of this test is implementation specific. */

static int verbose = 0;

int errfatal(int argc, char** argv) {
	int MY_ERROR_CLASS;
	int MY_ERROR_CODE;
	char MY_ERROR_STRING[10];
	
	sprintf(MY_ERROR_STRING, "MY ERROR");
	
	MPI_Init(&argc, &argv);
	
	if (verbose) 
	    printf("Adding My Error Class\n");
	MPI_Add_error_class(&MY_ERROR_CLASS);
	if (verbose) 
	    printf("Adding My Error Code\n");
	MPI_Add_error_code(MY_ERROR_CLASS, &MY_ERROR_CODE);
	if (verbose)
	    printf("Adding My Error String\n");
	MPI_Add_error_string(MY_ERROR_CODE, MY_ERROR_STRING);

	if (verbose)
	    printf("Calling Error Handler\n");
	MPI_Comm_call_errhandler(MPI_COMM_WORLD, MY_ERROR_CODE);

	/** We should not get here, because the default error handler
	   is ERRORS_ARE_FATAL.  This makes sure that the correct error 
	   handler is called and that no failure occured (such as 
	   a SEGV) in Comm_call_errhandler on the default 
	   error handler. */
	printf("After the Error Handler Has Been Called\n");

	MPI_Finalize();
	return 0;
}

}