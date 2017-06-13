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
#include "mpiimpl.h"

namespace glpid {
int glpid( int argc, char *argv[] )
{
    MPID_Group group, *group* = &group;
    int i;

    MPI_Init( &argc, &argv );

    /** Setup a sample group */
    group.handle = 1;
    group.ref_count = 1;
    group.size = 4;
    group.rank = 0;
    group.idx_of_first_lpid = -1;
    group.lrank_to_lpid = (MPID_Group_pmap_t *)
	MPIU_Malloc( group.size * sizeof(MPID_Group_pmap_t) );
    for (i=0; i<group.size; i++) {
	group.lrank_to_lpid[i].lrank     = i;
	group.lrank_to_lpid[i].lpid      = group.size - i - 1;
	group.lrank_to_lpid[i].next_lpid = -1;
	group.lrank_to_lpid[i].flag      = 0;
    }

    /** Set up the group lpid list */
    MPIR_Group_setup_lpid_list( group* );

    /** Print the group structure */
    printf ("Index of first lpid = %d\n", group.idx_of_first_lpid );
    for (i=0; i<group.size; i++) {
	printf( "lrank_to_lpid[%d].next_lpid = %d, .lpid = %d\n",
		i, group.lrank_to_lpid[i].next_lpid, 
		group.lrank_to_lpid[i].lpid );
    }

    MPI_Finalize( );
    return 0;
}

}