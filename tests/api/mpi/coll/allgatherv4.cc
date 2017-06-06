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
#include "mpitest.h"
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#include <math.h>
#include <assert.h>

namespace allgatherv4 {

/** FIXME: What is this test supposed to accomplish? */

#define START_BUF (1)
#define LARGE_BUF (8 * 1024)

/** FIXME: MAX_BUF is too large */
#define MAX_BUF   (8 * 1024 * 1024)
#define LOOPS 2

/** #define dprintf printf */
#define dprintf(...)

typedef enum {
    REGULAR,
    BCAST,
    SPIKE,
    HALF_FULL,
    LINEAR_DECREASE,
    BELL_CURVE
} test_t;

void comm_tests(MPI_Comm comm, char*,char*,int*,int*);
double run_test(long long msg_size, MPI_Comm comm, test_t test_type, double * max_time,
                char*,char*,int*,int*);

int allgatherv4(int argc, char ** argv)
{
  char * sbuf, * rbuf;
  int * recvcounts, * displs;
  int errs = 0;


    int comm_size, comm_rank;
    MPI_Comm comm;

    MTest_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);

    if (LARGE_BUF * comm_size > MAX_BUF)
        goto fn_exit;

    sbuf = (char *) calloc(MAX_BUF, 1);
    rbuf = (char *) calloc(MAX_BUF, 1);

    srand(time(NULL));

    recvcounts = (int *) malloc(comm_size * sizeof(int));
    displs = (int *) malloc(comm_size * sizeof(int));
    if (!recvcounts || !displs || !sbuf || !rbuf) {
        fprintf(stderr, "Unable to allocate memory:\n");
	if (!sbuf) fprintf(stderr,"\tsbuf of %d bytes\n", MAX_BUF );
	if (!rbuf) fprintf(stderr,"\trbuf of %d bytes\n", MAX_BUF );
	if (!recvcounts) fprintf(stderr,"\trecvcounts of %zd bytes\n", comm_size * sizeof(int) );
	if (!displs) fprintf(stderr,"\tdispls of %zd bytes\n", comm_size * sizeof(int) );
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    if (!comm_rank) {
        dprintf("Message Range: (%d, %d); System size: %d\n", START_BUF, LARGE_BUF, comm_size);
        fflush(stdout);
    }


    /** COMM_WORLD tests */
    if (!comm_rank) {
        dprintf("\n\n==========================================================\n");
        dprintf("                         MPI_COMM_WORLD\n");
        dprintf("==========================================================\n");
    }
    comm_tests(MPI_COMM_WORLD,sbuf,rbuf,recvcounts,displs);

    /** non-COMM_WORLD tests */
    if (!comm_rank) {
        dprintf("\n\n==========================================================\n");
        dprintf("                         non-COMM_WORLD\n");
        dprintf("==========================================================\n");
    }
    MPI_Comm_split(MPI_COMM_WORLD, (comm_rank == comm_size - 1) ? 0 : 1, 0, &comm);
    if (comm_rank < comm_size - 1)
        comm_tests(comm,sbuf,rbuf,recvcounts,displs);
    MPI_Comm_free(&comm);

    /** Randomized communicator tests */
    if (!comm_rank) {
        dprintf("\n\n==========================================================\n");
        dprintf("                         Randomized Communicator\n");
        dprintf("==========================================================\n");
    }
    MPI_Comm_split(MPI_COMM_WORLD, 0, rand(), &comm);
    comm_tests(comm, sbuf, rbuf, recvcounts, displs);
    MPI_Comm_free(&comm);

    free(sbuf);
    free(rbuf);
    free(recvcounts);
    free(displs);

fn_exit:
    MTest_Finalize(errs);
    MPI_Finalize();

    return 0;
}

void comm_tests(MPI_Comm comm, char* sbuf, char* rbuf, int* recvcounts, int* displs)
{
    int comm_size, comm_rank;
    double rtime, max_time;
    long long msg_size;

    MPI_Comm_size(comm, &comm_size);
    MPI_Comm_rank(comm, &comm_rank);

    for (msg_size = START_BUF; msg_size <= LARGE_BUF; msg_size *= 2) {
        if (!comm_rank) {
            dprintf("\n====> MSG_SIZE: %d\n", (int) msg_size);
            fflush(stdout);
        }

        rtime = run_test(msg_size, comm, REGULAR, &max_time, sbuf, rbuf, recvcounts, displs);
        if (!comm_rank) {
            dprintf("REGULAR:\tAVG: %.3f\tMAX: %.3f\n", rtime, max_time);
            fflush(stdout);
        }

        rtime = run_test(msg_size, comm, BCAST, &max_time, sbuf, rbuf, recvcounts, displs);
        if (!comm_rank) {
            dprintf("BCAST:\tAVG: %.3f\tMAX: %.3f\n", rtime, max_time);
            fflush(stdout);
        }

        rtime = run_test(msg_size, comm, SPIKE, &max_time, sbuf, rbuf, recvcounts, displs);
        if (!comm_rank) {
            dprintf("SPIKE:\tAVG: %.3f\tMAX: %.3f\n", rtime, max_time);
            fflush(stdout);
        }

        rtime = run_test(msg_size, comm, HALF_FULL, &max_time, sbuf, rbuf, recvcounts, displs);
        if (!comm_rank) {
            dprintf("HALF_FULL:\tAVG: %.3f\tMAX: %.3f\n", rtime, max_time);
            fflush(stdout);
        }

        rtime = run_test(msg_size, comm, LINEAR_DECREASE, &max_time, sbuf, rbuf, recvcounts, displs);
        if (!comm_rank) {
            dprintf("LINEAR_DECREASE:\tAVG: %.3f\tMAX: %.3f\n", rtime, max_time);
            fflush(stdout);
        }

        rtime = run_test(msg_size, comm, BELL_CURVE, &max_time, sbuf, rbuf, recvcounts, displs);
        if (!comm_rank) {
            dprintf("BELL_CURVE:\tAVG: %.3f\tMAX: %.3f\n", rtime, max_time);
            fflush(stdout);
        }
    }
}

double run_test(long long msg_size, MPI_Comm comm, test_t test_type, 
    double * max_time, char* sbuf, char* rbuf, int* recvcounts, int* displs)
{
    int i, j;
    int comm_size, comm_rank;
    double start, end;
    double total_time, avg_time;
    MPI_Aint tmp;

    MPI_Comm_size(comm, &comm_size);
    MPI_Comm_rank(comm, &comm_rank);

    displs[0] = 0;
    for (i = 0; i < comm_size; i++) {
        if (test_type == REGULAR)
            recvcounts[i] = msg_size;
        else if (test_type == BCAST)
            recvcounts[i] = (!i) ? msg_size : 0;
        else if (test_type == SPIKE)
            recvcounts[i] = (!i) ? (msg_size / 2) : (msg_size / (2 * (comm_size - 1)));
        else if (test_type == HALF_FULL)
            recvcounts[i] = (i < (comm_size / 2)) ? (2 * msg_size) : 0;
        else if (test_type == LINEAR_DECREASE) {
            tmp = 2 * msg_size * (comm_size - 1 - i) / (comm_size - 1);
	    if (tmp != (int)tmp) {
		fprintf( stderr, "Integer overflow in variable tmp\n" );
		MPI_Abort( MPI_COMM_WORLD, 1 );
	    }
            recvcounts[i] = (int) tmp;

            /** If the maximum message size is too large, don't run */
            if (tmp > MAX_BUF) return 0;
        }
        else if (test_type == BELL_CURVE) {
            for (j = 0; j < i; j++) {
                if (i - 1 + j >= comm_size) continue;
                tmp = msg_size * comm_size / (log(comm_size) * i);
                recvcounts[i - 1 + j] = (int) tmp;
                displs[i - 1 + j] = 0;

                /** If the maximum message size is too large, don't run */
                if (tmp > MAX_BUF) return 0;
            }
        }

        if (i < comm_size - 1)
            displs[i+1] = displs[i] + recvcounts[i];
    }

    /** Test that:
       1: sbuf is large enough
       2: rbuf is large enough
       3: There were no failures (e.g., tmp nowhere > rbuf size 
    */
    MPI_Barrier(comm);
    start = MPI_Wtime();
    for (i = 0; i < LOOPS; i++) {
        MPI_Allgatherv(sbuf, recvcounts[comm_rank], MPI_CHAR,
                       rbuf, recvcounts, displs, MPI_CHAR, comm);
    }
    end = MPI_Wtime();
    MPI_Barrier(comm);

    /** Convert to microseconds (why?) */
    total_time = 1.0e6 * (end - start);
    MPI_Reduce(&total_time, &avg_time, 1, MPI_DOUBLE, MPI_SUM, 0, comm);
    MPI_Reduce(&total_time, max_time, 1, MPI_DOUBLE, MPI_MAX, 0, comm);

    return (avg_time / (LOOPS * comm_size));
}
}