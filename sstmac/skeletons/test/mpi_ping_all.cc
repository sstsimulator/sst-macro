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

#include <sprockit/test/test.h>
#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/backtrace.h>
#include <sumi-mpi/mpi_api.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords("print_times",
                 "message_size",
                 "sleep_time");

#define sstmac_app_name mpi_ping_all

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(sumi::sstmac_mpi(), &sumi::transport::deadlock_check));
  sstmac::runtime::enter_deadlock_region();

  double t_start = MPI_Wtime();

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  //send/recv from all the other procs
  void* null_buffer = nullptr;
  sprockit::sim_parameters* params = get_params();
  int count = params->get_optional_byte_length_param("message_size", 100);
  bool print_times = params->get_optional_bool_param("print_times", true);
  int tag = 42;
  //one for each send, one for each recv
  MPI_Request* reqs = new MPI_Request[2*nproc];
  MPI_Request* reqptr = reqs;
  for (int i=0; i < nproc; ++i) {
    if (i == me) {
      continue;
    }

    MPI_Isend(null_buffer, count, MPI_BYTE, i, tag, MPI_COMM_WORLD, reqptr);
    ++reqptr;
    MPI_Irecv(null_buffer, count, MPI_BYTE, i, tag, MPI_COMM_WORLD, reqptr);
    ++reqptr;
  }
  int num_requests = 2*nproc - 2;

  // wait on the first quarter
  int quarter_size =  num_requests / 4;
  int remainder = num_requests % 4;

  double sleep_length = params->get_optional_time_param("sleep_time", 1);

  reqptr = reqs;
  for (int q=0; q < 4; ++q) {
    MPI_Waitall(quarter_size, reqptr, MPI_STATUSES_IGNORE);
    reqptr += quarter_size;
    sstmac_fsleep(sleep_length);
  }

  if (remainder) {
    MPI_Waitall(remainder, reqptr, MPI_STATUSES_IGNORE);
    sstmac_fsleep(sleep_length);
  }

  double t_stop = MPI_Wtime();
  double t_total = t_stop - t_start;
  if (print_times) ::printf("Rank %d = %8.4fms\n", me, t_total*1e3);

  delete[] reqs;

  MPI_Finalize();
  sstmac::runtime::exit_deadlock_region();
  return 0;
}