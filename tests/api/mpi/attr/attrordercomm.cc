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
#include "mpitest.h"

/**
static char MTestDescrip[] = "Test creating and inserting attributes in \
different orders to ensure that the list management code handles all cases.";
*/

namespace attrordercomm {

int checkAttrs( MPI_Comm comm, int n, int key[], int attrval[] );
int checkNoAttrs( MPI_Comm comm, int n, int key[] );

int attrordercomm( int argc, char *argv[] )
{
    int errs = 0;
    int key[3], attrval[3];
    int i;
    MPI_Comm comm;

    MTest_Init( &argc, &argv );

    {
	comm = MPI_COMM_WORLD;
	/** Create key values */
	for (i=0; i<3; i++) {
	    MPI_Comm_create_keyval( MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN,
			       &key[i], (void *)0 );
	    attrval[i] = 1024 * i;
	}
	
	/** Insert attribute in several orders.  Test after put with get,
	 then delete, then confirm delete with get. */

	MPI_Comm_set_attr( comm, key[2], &attrval[2] );
	MPI_Comm_set_attr( comm, key[1], &attrval[1] );
	MPI_Comm_set_attr( comm, key[0], &attrval[0] );

	errs += checkAttrs( comm, 3, key, attrval );
	
	MPI_Comm_delete_attr( comm, key[0] );
	MPI_Comm_delete_attr( comm, key[1] );
	MPI_Comm_delete_attr( comm, key[2] );

	errs += checkNoAttrs( comm, 3, key );
	
	MPI_Comm_set_attr( comm, key[1], &attrval[1] );
	MPI_Comm_set_attr( comm, key[2], &attrval[2] );
	MPI_Comm_set_attr( comm, key[0], &attrval[0] );

	errs += checkAttrs( comm, 3, key, attrval );
	
	MPI_Comm_delete_attr( comm, key[2] );
	MPI_Comm_delete_attr( comm, key[1] );
	MPI_Comm_delete_attr( comm, key[0] );

	errs += checkNoAttrs( comm, 3, key );

	MPI_Comm_set_attr( comm, key[0], &attrval[0] );
	MPI_Comm_set_attr( comm, key[1], &attrval[1] );
	MPI_Comm_set_attr( comm, key[2], &attrval[2] );

	errs += checkAttrs( comm, 3, key, attrval );
	
	MPI_Comm_delete_attr( comm, key[1] );
	MPI_Comm_delete_attr( comm, key[2] );
	MPI_Comm_delete_attr( comm, key[0] );

	errs += checkNoAttrs( comm, 3, key );
	
	for (i=0; i<3; i++) {
	    MPI_Comm_free_keyval( &key[i] );
	}
    }
    
    MTest_Finalize( errs );
    MPI_Finalize();
    return 0;
  
}

int checkAttrs( MPI_Comm comm, int n, int key[], int attrval[] )
{
    int errs = 0;
    int i, flag, *val_p;

    for (i=0; i<n; i++) {
	MPI_Comm_get_attr( comm, key[i], &val_p, &flag );
	if (!flag) {
	    errs++;
	    fprintf( stderr, "Attribute for key %d not set\n", i );
	}
	else if (val_p != &attrval[i]) {
	    errs++;
	    fprintf( stderr, "Atribute value for key %d not correct\n",
		     i );
	}
    }

    return errs;
}

int checkNoAttrs( MPI_Comm comm, int n, int key[] )
{
    int errs = 0;
    int i, flag, *val_p;

    for (i=0; i<n; i++) {
	MPI_Comm_get_attr( comm, key[i], &val_p, &flag );
	if (flag) {
	    errs++;
	    fprintf( stderr, "Attribute for key %d set but should be deleted\n", i );
	}
    }

    return errs;
}
	
}