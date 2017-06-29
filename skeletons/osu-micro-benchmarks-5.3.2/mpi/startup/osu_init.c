#define BENCHMARK "OSU MPI Init Test"
#ifdef PACKAGE_VERSION
#   define HEADER "# " BENCHMARK " v" PACKAGE_VERSION "\n"
#else
#   define HEADER "# " BENCHMARK "\n"
#endif
/*
 * Copyright (C) 2002-2016 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
 *
 * For detailed copyright and licensing information, please refer to the
 * copyright file COPYRIGHT in the top level OMB directory.
 */
#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#ifdef __MACH__ 
#include <mach/clock.h>
#include <mach/mach.h>
#define portable_gettime(ts) \
  { \
  clock_serv_t cclock; \
  mach_timespec_t mts; \
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock); \
  clock_get_time(cclock, &mts); \
  mach_port_deallocate(mach_task_self(), cclock); \
  ts.tv_sec = mts.tv_sec; \
  ts.tv_nsec = mts.tv_nsec;  \
  }
#else
#define portable_gettime(ts) clock_gettime(CLOCK_REALTIME, &ts); 
#endif

int
main (int argc, char *argv[])
{
    int myid, numprocs;
    struct timespec tp_before, tp_after;
    long duration = 0, min, max, avg;

    portable_gettime(tp_before);
    MPI_Init(&argc, &argv);
    portable_gettime(tp_after);

    duration = (tp_after.tv_sec - tp_before.tv_sec) * 1e3;
    duration += (tp_after.tv_nsec - tp_before.tv_nsec) / 1e6;

    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);

    MPI_Reduce(&duration, &min, 1, MPI_LONG, MPI_MIN, 0, MPI_COMM_WORLD);
    MPI_Reduce(&duration, &max, 1, MPI_LONG, MPI_MAX, 0, MPI_COMM_WORLD);
    MPI_Reduce(&duration, &avg, 1, MPI_LONG, MPI_SUM, 0, MPI_COMM_WORLD);
    avg = avg/numprocs;

    if(myid == 0) {
        fprintf(stdout, HEADER);
        fprintf(stdout, "nprocs: %d, min: %ld ms, max: %ld ms, avg: %ld ms\n",
                numprocs, min, max, avg);
        fflush(stdout);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}

