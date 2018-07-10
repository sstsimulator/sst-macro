/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

namespace sumi {

bool
mpi_api::test(MPI_Request *request, MPI_Status *status)
{
  _start_mpi_call_(MPI_Test);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Test(...)");

  if (*request == MPI_REQUEST_NULL){
    return true;
  }

  mpi_request* reqPtr = get_request(*request);
  if (reqPtr->is_complete()){
    if (status != MPI_STATUS_IGNORE){
      *status = reqPtr->status();
    }
    erase_request_ptr(*request);
    *request = MPI_REQUEST_NULL;
    return true;
  } else {
    if (test_delay_us_){
      queue_->forward_progress(test_delay_us_*1e-6);
    }
    return false;
  }
}

int
mpi_api::test(MPI_Request *request, int *flag, MPI_Status *status)
{
  MPI_Request req_cpy = *request;
  auto call_start_time = (uint64_t)os_->now().usec();

  _start_mpi_call_(MPI_Test);
  if (test(request, status)){
    mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Test(...)");
    *flag = 1;
  } else {
    *flag = 0;
  }
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    otf2_writer_.mpi_test(comm_world()->rank(),
                          call_start_time,
                          (uint64_t)os_->now().usec(),
                          req_cpy,
                          *flag);
  }
#endif

  return MPI_SUCCESS;
}

int
mpi_api::testall(int count, MPI_Request array_of_requests[], int *flag, MPI_Status array_of_statuses[])
{
#ifdef SSTMAC_OTF2_ENABLED
  auto call_start_time = (uint64_t)os_->now().usec();

  // Make a copy of requests since they may mutate before OTF2 call
  std::vector<dumpi::request_t> requests(array_of_requests, array_of_requests + count);
#endif

  _start_mpi_call_(MPI_Testall);
  *flag = 1;
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  for (int i=0; i < count; ++i){
    MPI_Status* stat = ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i];
    if (!test(&array_of_requests[i], stat)){
      *flag = 0;
    }
  }
  if (*flag){
    mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request,
      "MPI_Testall(%d,...)", count);
  }
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    otf2_writer_.mpi_testall(comm_world()->rank(),
                          call_start_time,
                          (uint64_t)os_->now().usec(),
                          count,
                          requests.data(),
                          *flag);
  }
#endif

  return MPI_SUCCESS;
}

int
mpi_api::testany(int count, MPI_Request array_of_requests[], int *indx, int *flag, MPI_Status *status)
{
#ifdef SSTMAC_OTF2_ENABLED
  auto call_start_time = (uint64_t)os_->now().usec();

  // Make a copy of requests since they may mutate before OTF2 call
  std::vector<dumpi::request_t> requests(array_of_requests, array_of_requests + count);
#endif

  start_api_call();
  if (count == 0){
    *flag = 1;
    return MPI_SUCCESS;
  }
  *flag = 0;
  *indx = MPI_UNDEFINED;
  for (int i=0; i < count; ++i){
    if (test(&array_of_requests[i], status)){
      *flag = 1;
      *indx = i;
      return MPI_SUCCESS;
    }
  }
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    otf2_writer_.mpi_testany(comm_world()->rank(),
                          call_start_time,
                          (uint64_t)os_->now().usec(),
                          requests.data(),
                          *indx,
                          *flag);
  }
#endif

  return MPI_SUCCESS;
}

int
mpi_api::testsome(int incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[])
{
#ifdef SSTMAC_OTF2_ENABLED
  auto call_start_time = (uint64_t)os_->now().usec();

  // Make a copy of requests since they may mutate before OTF2 call
  std::vector<dumpi::request_t> requests(array_of_requests, array_of_requests + incount);
#endif

  start_api_call();
  int numComplete = 0;
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  for (int i=0; i < incount; ++i){
    MPI_Status* stat = ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i];
    if (test(&array_of_requests[i], stat)){
      array_of_indices[numComplete++] = i;
    }
  }
  *outcount = numComplete;
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    std::vector<int> statuses;

    otf2_writer_.mpi_testsome(comm_world()->rank(),
                          call_start_time,
                          (uint64_t)os_->now().usec(),
                          requests.data(),
                          *outcount,
                          array_of_indices);
  }
#endif

  return MPI_SUCCESS;
}


}
