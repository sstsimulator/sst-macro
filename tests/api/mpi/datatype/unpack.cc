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
#include <stdlib.h>
#include <string.h>

namespace unpack {

/** Test sent in by Avery Ching to report a bug in MPICH2. 
   Adding it as a regression test. */

/**
static void print_char_buf(char *buf_name, char *buf, int buf_len)
{
    int i;

    printf("print_char_buf: %s\n", buf_name);
    for (i = 0; i < buf_len; i++)
    {
        printf("%c ", buf[i]);
        if (((i + 1) % 10) == 0)
            printf("\n");
        else if (((i + 1) % 5) == 0)
            printf("  ");
    }
    printf("\n");
}
*/

char correct_buf[] = {'a', '_', 'b', 'c', '_', '_', '_', '_', 'd', '_', 
		      'e', 'f', 'g', '_', 'h', 'i', 'j', '_', 'k', 'l',
		      '_', '_', '_', '_', 'm', '_', 'n', 'o', 'p', '_',
		      'q', 'r'};

#define COUNT 2

int unpack(int argc, char **argv)
{
    int myid, numprocs, i;
    char *mem_buf = NULL, *unpack_buf = NULL;
    MPI_Datatype tmp_dtype, mem_dtype;
    MPI_Aint mem_dtype_ext = -1;
    int mem_dtype_sz = -1;
    int mem_buf_sz = -1, unpack_buf_sz = -1, buf_pos = 0;

    int blk_arr[COUNT] = {1, 2};
    int dsp_arr[COUNT] = {0, 2};
    int errs = 0;

    MTest_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    /** Creating the datatype to use for unpacking */
    MPI_Type_indexed(COUNT, blk_arr, dsp_arr,
                     MPI_CHAR, &tmp_dtype);
    MPI_Type_commit(&tmp_dtype);
    MPI_Type_indexed(COUNT, blk_arr, dsp_arr,
                     tmp_dtype, &mem_dtype);
    MPI_Type_free( &tmp_dtype );
    MPI_Type_commit(&mem_dtype);

    MPI_Type_size(mem_dtype, &mem_dtype_sz);
    MPI_Type_extent(mem_dtype, &mem_dtype_ext);

    mem_buf_sz    = 2 * mem_dtype_ext;
    unpack_buf_sz = 2 * mem_dtype_sz;

    if ((mem_buf = (char *) malloc(mem_buf_sz)) == NULL)
    {
	fprintf(stderr, "malloc mem_buf of size %d failed\n", mem_buf_sz);
	return -1;
    }
    memset(mem_buf, '_', mem_buf_sz);

    if ((unpack_buf = (char *) malloc(unpack_buf_sz)) == NULL)
    {
	fprintf(stderr, "malloc unpack_buf of size %d failed\n", 
		unpack_buf_sz);
	return -1;
    }
    
    for (i = 0; i < unpack_buf_sz; i++)
	unpack_buf[i] = 'a' + i;
    
    /** print_char_buf("mem_buf before unpack", mem_buf, 2 * mem_dtype_ext); */

    MPI_Unpack(unpack_buf, unpack_buf_sz, &buf_pos,
	       mem_buf, 2, mem_dtype, MPI_COMM_SELF);
    /** Note: Unpack without a Pack is not technically correct, but should work
     * with MPICH2. */

    /** print_char_buf("mem_buf after unpack", mem_buf, 2 * mem_dtype_ext);
       print_char_buf("correct buffer should be", 
                       correct_buf, 2 * mem_dtype_ext); */

    if (memcmp(mem_buf, correct_buf, 2 * mem_dtype_ext)) {
	printf("Unpacked buffer does not match expected buffer\n");
	errs++;
    }

    MPI_Type_free(&mem_dtype);

    MTest_Finalize(errs);
    MPI_Finalize();

    return 0;
}

}