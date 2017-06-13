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

namespace fkeyvalcomm {

/**
static char MTestDescrip[] = "Test freeing keyvals while still attached to \
a communicator, then make sure that the keyval delete and copy code are still \
executed";
*/

/** Function prototypes to keep compilers happy */
int copy_fn( MPI_Comm oldcomm, int keyval, void *extra_state,
	     void *attribute_val_in, void *attribute_val_out, 
	     int *flag);
int delete_fn( MPI_Comm comm, int keyval, void *attribute_val, 
	       void *extra_state);

/** Copy increments the attribute value */
int copy_fn( MPI_Comm oldcomm, int keyval, void *extra_state,
	     void *attribute_val_in, void *attribute_val_out, 
	     int *flag)
{
    /** Copy the address of the attribute */
    *(void **)attribute_val_out = attribute_val_in;
    /** Change the value */
    *(int *)attribute_val_in = *(int *)attribute_val_in + 1;
    /** set flag to 1 to tell comm dup to insert this attribute
       into the new communicator */
    *flag = 1;
    return MPI_SUCCESS;
}

/** Delete decrements the attribute value */
int delete_fn( MPI_Comm comm, int keyval, void *attribute_val, 
	       void *extra_state)
{
    *(int *)attribute_val = *(int *)attribute_val - 1;
    return MPI_SUCCESS;
}

int fkeyvalcomm( int argc, char *argv[] )
{
    int errs = 0;
    int attrval;
    int i, key[32], keyval, saveKeyval;
    MPI_Comm comm, dupcomm;
    MTest_Init( &argc, &argv );

    while (MTestGetIntracomm( &comm, 1 )) {
	if (comm == MPI_COMM_NULL) continue;

	MPI_Comm_create_keyval( copy_fn, delete_fn, &keyval, (void *)0 );
	saveKeyval = keyval;   /** in case we need to free explicitly */
	attrval = 1;
	MPI_Comm_set_attr( comm, keyval, (void*)&attrval );
	/** See MPI-1, 5.7.1.  Freeing the keyval does not remove it if it
	   is in use in an attribute */
	MPI_Comm_free_keyval( &keyval );
	
	/** We create some dummy keyvals here in case the same keyval
	   is reused */
	for (i=0; i<32; i++) {
	    MPI_Comm_create_keyval( MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN,
			       &key[i], (void *)0 );
	}

	MPI_Comm_dup( comm, &dupcomm );
	/** Check that the attribute was copied */
	if (attrval != 2) {
	    errs++;
	    printf( "Attribute not incremented when comm dup'ed (%s)\n",
		    MTestGetIntracommName() );
	}
	MPI_Comm_free( &dupcomm );
	if (attrval != 1) {
	    errs++;
	    printf( "Attribute not decremented when dupcomm %s freed\n",
		    MTestGetIntracommName() );
	}
	/** Check that the attribute was freed in the dupcomm */

	if (comm != MPI_COMM_WORLD && comm != MPI_COMM_SELF) {
	    MPI_Comm_free( &comm );
	    /** Check that the original attribute was freed */
	    if (attrval != 0) {
		errs++;
		printf( "Attribute not decremented when comm %s freed\n",
			MTestGetIntracommName() );
	    }
	}
	else {
	    /** Explicitly delete the attributes from world and self */
	    MPI_Comm_delete_attr( comm, saveKeyval );
	}
	/** Free those other keyvals */
	for (i=0; i<32; i++) {
	    MPI_Comm_free_keyval( &key[i] );
	}
    }
    MTest_Finalize( errs );
    MPI_Finalize();

    /** The attributes on comm self and world were deleted by finalize 
       (see separate test) */
    
    return 0;
  
}

}