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
/** stdlib.h Needed for malloc declaration */
#include <stdlib.h>

namespace groupcreate {

int groupcreate( int argc, char **argv )
{
    int i, n, n_goal = 2048, n_all, rc, n_ranks, *ranks, rank, size, len;
    int group_size;
    MPI_Group *group_array, world_group;
    char msg[MPI_MAX_ERROR_STRING];

    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    n = n_goal;
    
    group_array = (MPI_Group *)malloc( n * sizeof(MPI_Group) );

    MPI_Comm_group( MPI_COMM_WORLD, &world_group );

    n_ranks = size;
    ranks = (int *)malloc( size * sizeof(int) );
    for (i=0; i<size; i++) ranks[i] = i;

    MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_RETURN );
    for (i=0; i<n; i++) {
	rc = MPI_Group_incl( world_group, n_ranks, ranks, group_array + i );
 	if (rc) {
	    fprintf( stderr, "Error when creating group number %d\n", i );
	    MPI_Error_string( rc, msg, &len );
	    fprintf( stderr, "%s\n", msg );
	    n = i + 1;
	    break;
	}
	else {
	    /** Check that the group was created (and that any errors were
	       caught) */
	    rc = MPI_Group_size( group_array[i], &group_size );
	    if (group_size != size) {
		fprintf( stderr, "Group number %d not correct (size = %d)\n", 
			 i, size );
		n = i + 1; 
		break;
	    }
	}
	
    }

    for (i=0; i<n; i++) {
	rc = MPI_Group_free( group_array + i );
	if (rc) {
	    fprintf( stderr, "Error when freeing group number %d\n", i );
	    MPI_Error_string( rc, msg, &len );
	    fprintf( stderr, "%s\n", msg );
	    break;
	}
    }

    MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL );
    MPI_Group_free( &world_group );

    MPI_Reduce( &n, &n_all, 1, MPI_INT, MPI_MIN, 0, MPI_COMM_WORLD );
    if (rank == 0) {
	/** printf( "Completed test of %d type creations\n", n_all ); */
	if (n_all != n_goal) {
	    printf (
"This MPI implementation limits the number of groups that can be created\n\
This is allowed by the standard and is not a bug, but is a limit on the\n\
implementation\n" );
	}
	else {
	    printf( " No Errors\n" );
	}
    }

    free( group_array );

    MPI_Finalize( );
    return 0;
}
}