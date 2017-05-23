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

namespace contigstruct {
/**
 * This test checks to see if we can create a simple datatype
 * made from many contiguous copies of a single struct.  The
 * struct is built with monotone decreasing displacements to
 * avoid any struct->contig optimizations.
 */

int contigstruct( int argc, char **argv )
{
    int           blocklens[8], psize, i, rank;
    MPI_Aint      displs[8];
    MPI_Datatype  oldtypes[8];
    MPI_Datatype  ntype1, ntype2;

    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    
    for (i=0; i<8; i++) {
	blocklens[i] = 1;
	displs[i]    = (7-i) * sizeof(long);
	oldtypes[i]  = MPI_LONG;
    }
    MPI_Type_struct( 8, blocklens, displs, oldtypes, &ntype1 );
    MPI_Type_contiguous( 65536, ntype1, &ntype2 );
    MPI_Type_commit( &ntype2 );

    MPI_Pack_size( 2, ntype2, MPI_COMM_WORLD, &psize );

    MPI_Type_free( &ntype2 );
	MPI_Type_free( &ntype1 );

    /** The only failure mode has been SEGV or aborts within the datatype
       routines */
    if (rank == 0) {
	printf( " No Errors\n" );
    }

    MPI_Finalize();
    return 0;
}

}