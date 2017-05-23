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
#include <string.h>
#include "mpitest.h"

namespace commcreate1 {
/** Check that Communicators can be created from various subsets of the
   processes in the communicator.
*/

void abortMsg( const char *, int );
int BuildComm( MPI_Comm, MPI_Group, const char [] );

void abortMsg( const char *str, int code )
{
    char msg[MPI_MAX_ERROR_STRING];
    int cl, resultLen;

    MPI_Error_class( code, &cl );
    MPI_Error_string( code, msg, &resultLen );
    fprintf( stderr, "%s: errcode = %d, class = %d, msg = %s\n", 
	     str, code, cl, msg );
    MPI_Abort( MPI_COMM_WORLD, code );
}

int commcreate1( int argc, char *argv[] )
{
    MPI_Comm  dupWorld;
    int       wrank, wsize, gsize, err, errs = 0;
    int       ranges[1][3];
    MPI_Group wGroup, godd, ghigh, geven;

    MTest_Init( &argc, &argv );

    MPI_Comm_size( MPI_COMM_WORLD, &wsize );
    MPI_Comm_rank( MPI_COMM_WORLD, &wrank );

    /** Create some groups */
    MPI_Comm_group( MPI_COMM_WORLD, &wGroup );

    MTestPrintfMsg( 2, "Creating groups\n" );
    ranges[0][0] = 2*(wsize/2)-1;
    ranges[0][1] = 1;
    ranges[0][2] = -2;
    err = MPI_Group_range_incl( wGroup, 1, ranges, &godd );
    if (err) abortMsg( "Failed to create odd group: ", err );
    err = MPI_Group_size( godd, &gsize );
    if (err) abortMsg( "Failed to get size of odd group: ", err );
    if (gsize != wsize/2) {
	fprintf( stderr, "Group godd size is %d should be %d\n", gsize, 
		 wsize/2 );
	errs++;
    }

    ranges[0][0] = wsize/2+1;
    ranges[0][1] = wsize-1;
    ranges[0][2] = 1;
    err = MPI_Group_range_incl( wGroup, 1, ranges, &ghigh );
    if (err) abortMsg( "Failed to create high group\n", err );
    ranges[0][0] = 0;
    ranges[0][1] = wsize-1;
    ranges[0][2] = 2;
    err = MPI_Group_range_incl( wGroup, 1, ranges, &geven );
    if (err) abortMsg( "Failed to create even group:", err );

    MPI_Comm_dup( MPI_COMM_WORLD, &dupWorld );
    MPI_Comm_set_name( dupWorld, (char*)"Dup of world" );
    /** First, use the groups to create communicators from world and a dup
       of world */
    errs += BuildComm( MPI_COMM_WORLD, ghigh, "ghigh" );
    errs += BuildComm( MPI_COMM_WORLD, godd, "godd" );
    errs += BuildComm( MPI_COMM_WORLD, geven, "geven" );
    errs += BuildComm( dupWorld, ghigh, "ghigh" );
    errs += BuildComm( dupWorld, godd, "godd" );
    errs += BuildComm( dupWorld, geven, "geven" );

#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
    /** check that we can create multiple communicators from a single collective
     * call to MPI_Comm_create as long as the groups are all disjoint */
    errs += BuildComm( MPI_COMM_WORLD, (wrank % 2 ? godd : geven), "godd+geven" );
    errs += BuildComm( dupWorld,       (wrank % 2 ? godd : geven), "godd+geven" );
    errs += BuildComm( MPI_COMM_WORLD, MPI_GROUP_EMPTY, "MPI_GROUP_EMPTY" );
    errs += BuildComm( dupWorld,       MPI_GROUP_EMPTY, "MPI_GROUP_EMPTY" );
#endif

    MPI_Comm_free( &dupWorld );
    MPI_Group_free( &ghigh );
    MPI_Group_free( &godd );
    MPI_Group_free( &geven );
    MPI_Group_free( &wGroup );

    MTest_Finalize( errs );

    MPI_Finalize();
    return 0;
}

int BuildComm( MPI_Comm oldcomm, MPI_Group group, const char gname[] )
{
    MPI_Comm newcomm;
    int grank, gsize, rank, size, errs = 0;
    char cname[MPI_MAX_OBJECT_NAME+1];
    int  cnamelen;

    MPI_Group_rank( group, &grank );
    MPI_Group_size( group, &gsize );
    MPI_Comm_get_name( oldcomm, cname, &cnamelen );
    MTestPrintfMsg( 2, "Testing comm %s from %s\n", cname, gname );
    MPI_Comm_create( oldcomm, group, &newcomm );
    if (newcomm == MPI_COMM_NULL && grank != MPI_UNDEFINED) {
	errs ++;
	fprintf( stderr, "newcomm is null but process is in group\n" );
    }
    if (newcomm != MPI_COMM_NULL && grank == MPI_UNDEFINED) {
	errs ++;
	fprintf( stderr, "newcomm is not null but process is not in group\n" );
    }
    if (newcomm != MPI_COMM_NULL && grank != MPI_UNDEFINED) {
	MPI_Comm_rank( newcomm, &rank );
	if (rank != grank) {
	    errs ++;
	    fprintf( stderr, "Rank is %d should be %d in comm from %s\n", 
		     rank, grank, gname );
	}
	MPI_Comm_size( newcomm, &size );
	if (size != gsize) {
	    errs++;
	    fprintf( stderr, "Size is %d should be %d in comm from %s\n",
		     size, gsize, gname );
	}
	MPI_Comm_free( &newcomm );
	MTestPrintfMsg( 2, "Done testing comm %s from %s\n", cname, gname );
    }
    return errs;
}

}