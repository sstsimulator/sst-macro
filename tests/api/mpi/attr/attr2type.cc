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


namespace attr2type {

static int foo_keyval = MPI_KEYVAL_INVALID;

int foo_initialize(void);
void foo_finalize(void);

int foo_copy_attr_function(MPI_Datatype type, int type_keyval,
			   void *extra_state, void *attribute_val_in,
			   void *attribute_val_out, int *flag);
int foo_delete_attr_function(MPI_Datatype type, int type_keyval,
			     void *attribute_val, void *extra_state);
static const char *my_func = 0;
static int verbose = 0;
static int delete_called = 0;
static int copy_called = 0;

int attr2type(int argc, char *argv[])
{
    int mpi_errno;
    MPI_Datatype type, duptype;
    int rank;

    MPI_Init(&argc, &argv);
    
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );

    foo_initialize();

    mpi_errno = MPI_Type_contiguous(2, MPI_INT, &type);

    mpi_errno = MPI_Type_set_attr(type, foo_keyval, NULL);

    mpi_errno = MPI_Type_dup(type, &duptype);

    my_func = "Free of type";
    mpi_errno = MPI_Type_free(&type);

    my_func = "free of duptype";
    mpi_errno = MPI_Type_free(&duptype);

    foo_finalize();

    if (rank == 0) {
      int errs = 0;
      if (copy_called != 1) {
	printf( "Copy called %d times; expected once\n", copy_called );
	errs++;
      }
      if (delete_called != 2) {
	printf( "Delete called %d times; expected twice\n", delete_called );
	errs++;
      }
      if (errs == 0) {
	printf( " No Errors\n" );
      }
      else {
	printf( " Found %d errors\n", errs );
      }
      fflush(stdout);
    }

    MPI_Finalize();
    return 0;
}

int foo_copy_attr_function(MPI_Datatype type,
			   int type_keyval,
			   void *extra_state,
			   void *attribute_val_in,
			   void *attribute_val_out,
			   int *flag)
{
    if (verbose) printf("copy fn. called\n");
    copy_called ++;
    * (char **) attribute_val_out = NULL;
    *flag = 1;

    return MPI_SUCCESS;
}

int foo_delete_attr_function(MPI_Datatype type,
			     int type_keyval,
			     void *attribute_val,
			     void *extra_state)
{
    if (verbose) printf("delete fn. called in %s\n", my_func );
    delete_called ++;

    return MPI_SUCCESS;
}

int foo_initialize(void)
{
    int mpi_errno;

    /** create keyval for use later */
    mpi_errno = MPI_Type_create_keyval(foo_copy_attr_function,
				       foo_delete_attr_function,
				       &foo_keyval,
				       NULL);
    if (verbose) printf("created keyval\n");

    return 0;
}

void foo_finalize(void)
{
    int mpi_errno;

    /** remove keyval */
    mpi_errno = MPI_Type_free_keyval(&foo_keyval);

    if (verbose) printf("freed keyval\n");

    return;
}

}