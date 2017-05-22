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

#ifndef MPITEST_H_INCLUDED
#define MPITEST_H_INCLUDED

#include "mpitestconf.h"

/*
 * Init and finalize test 
 */
void MTest_Init( int *, char *** );
void MTest_Init_thread( int *, char ***, int, int * );
void MTest_Finalize( int );
void MTestPrintError( int );
void MTestPrintErrorMsg( const char [], int );
void MTestPrintfMsg( int, const char [], ... );
void MTestError( const char [] );
int MTestReturnValue( int );

/*
 * Utilities
 */
void MTestSleep( int );

/*
 * This structure contains the information used to test datatypes
 * buf is set to null when an MTestDatatype is created; the
 * InitBuf routine will allocate (if necessary) and initialize
 * the data.  InitBuf may be called multiple times (this is particularly
 * important for recv bufs), in which case the buffer will only 
 * be allocated if it has not already been created.
 */
typedef struct _MTestDatatype {
    MPI_Datatype datatype;
    void *buf;              /* buffer to use in communication */
    int  count;             /* count to use for this datatype */
    int  isBasic;           /* true if the type is predefined */
    int  printErrors;       /* true if errors should be printed
			       (used by the CheckBuf routines) */
    /* The following is optional data that is used by some of
       the derived datatypes */
    int  stride, nelm, blksize, *index;
    /* stride, nelm, and blksize are in bytes */
    int *displs, basesize;
    /* displacements are in multiples of base type; basesize is the
       size of that type*/
    void *(*InitBuf)( struct _MTestDatatype * );
    void *(*FreeBuf)( struct _MTestDatatype * );
    int   (*CheckBuf)( struct _MTestDatatype * );
} MTestDatatype;

int MTestCheckRecv( MPI_Status *, MTestDatatype * );
int MTestGetDatatypes( MTestDatatype *, MTestDatatype *, int );
void MTestResetDatatypes( void );
void MTestFreeDatatype( MTestDatatype * );
const char *MTestGetDatatypeName( MTestDatatype * );
int MTestGetDatatypeIndex( void );

int MTestGetIntracomm( MPI_Comm *, int );
int MTestGetIntracommGeneral( MPI_Comm *, int, int );
int MTestGetComm( MPI_Comm *, int );
const char *MTestGetIntracommName( void );
void MTestFreeComm( MPI_Comm * );

#ifdef HAVE_MPI_WIN_CREATE
int MTestGetWin( MPI_Win *, int );
const char *MTestGetWinName( void );
void MTestFreeWin( MPI_Win * );
#endif

/* These macros permit overrides via:
 *     make CPPFLAGS='-DMTEST_MPI_VERSION=X -DMTEST_MPI_SUBVERSION=Y'
 * where X and Y are the major and minor versions of the MPI standard that is
 * being tested.  The override should work similarly if added to the CPPFLAGS at
 * configure time. */
#ifndef MTEST_MPI_VERSION
#define MTEST_MPI_VERSION MPI_VERSION
#endif
#ifndef MTEST_MPI_SUBVERSION
#define MTEST_MPI_SUBVERSION MPI_SUBVERSION
#endif

/* Makes it easier to conditionally compile test code for a particular minimum
 * version of the MPI Standard.  Right now there is only a MIN flavor but it
 * would be easy to add MAX or EXACT flavors if they become necessary at some
 * point.  Example usage:
 ------8<-------
#if MTEST_HAVE_MIN_MPI_VERSION(2,2)
... test for some feature that is only available in MPI-2.2 or later ...
#endif
 ------8<-------
 */
#define MTEST_HAVE_MIN_MPI_VERSION(major_,minor_) \
    ((MTEST_MPI_VERSION == (major_) && MTEST_MPI_SUBVERSION >= (minor_)) ||   \
    (MTEST_MPI_VERSION > (major_)))

#endif