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
#include <cassert>

namespace sumi {

int
mpi_api::wait(MPI_Request *request, MPI_Status *status)
{
#ifdef SSTMAC_OTF2_ENABLED
  auto call_start_time = (uint64_t)os_->now().usec();
  MPI_Request request_cpy = *request;
#endif

  start_mpi_call(MPI_Wait);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Wait(...)");
  int rc = do_wait(request, status);
  finish_mpi_call(MPI_Wait);

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    otf2_writer_.mpi_wait(comm_world()->rank(),
                          call_start_time,
                          (uint64_t)os_->now().usec(),
                          request_cpy);
  }
#endif
  return rc;
}

int
mpi_api::do_wait(MPI_Request *request, MPI_Status *status)
{
  MPI_Request req = *request;
  if (req == MPI_REQUEST_NULL){
    return MPI_SUCCESS;
  }

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, 
    "   MPI_Wait_nonnull(%d)", req);

  mpi_request* reqPtr = get_request(req);
  if (!reqPtr->is_complete()){
   queue_->progress_loop(reqPtr);
  }

  finalize_wait_request(reqPtr, request, status);

  return MPI_SUCCESS;
}

void
mpi_api::finalize_wait_request(mpi_request* reqPtr, MPI_Request* req, MPI_Status* status)
{
  if (status != MPI_STATUS_IGNORE){
    *status = reqPtr->status();
  }
  if (!reqPtr->is_persistent()){
    req_map_.erase(*req);
    *req = MPI_REQUEST_NULL;
    delete reqPtr;
  }
}

int
mpi_api::waitall(int count, MPI_Request array_of_requests[],
                 MPI_Status array_of_statuses[])
{
  auto call_start_time = (uint64_t)os_->now().usec();

  start_mpi_call(MPI_Waitall);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, 
    "MPI_Waitall(%d,...)", count);
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  int last_nonnull = 0;
  std::vector<MPI_Request> req_vec;
  for (int i=0; i < count; ++i){
    MPI_Status* status = ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i];
    if (array_of_requests[i] != MPI_REQUEST_NULL){
      last_nonnull = i;
    }
    req_vec.push_back(array_of_requests[i]);
    do_wait(&array_of_requests[i], status);
  }
  finish_mpi_call(MPI_Waitall);

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    otf2_writer_.mpi_waitall(comm_world()->rank(),
                             call_start_time,
                             (uint64_t)os_->now().usec(),
                             count,
                             req_vec.data());
  }
#endif

  return MPI_SUCCESS;
}

int
mpi_api::waitany(int count, MPI_Request array_of_requests[], int *indx,
                 MPI_Status *status)
{
  auto call_start_time = (uint64_t)os_->now().usec();
  bool call_completed = false;

  // for caching the request in an upper scope before it is destroyed
  MPI_Request req_val = -1;

  start_mpi_call(MPI_Waitany);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Waitany(...)");
  *indx = MPI_UNDEFINED;
  std::vector<mpi_request*> reqPtrs(count);
  int numNonnull = 0;
  //use a null ptr internally to indicate ignore
  for (int i=0; i < count; ++i){
    MPI_Request req = array_of_requests[i];
    if (req != MPI_REQUEST_NULL){
      mpi_request* reqPtr = get_request(req);
      if (reqPtr->is_complete()){
        *indx = i;
        req_val = req;
        finalize_wait_request(reqPtr, &array_of_requests[i], status);
        finish_mpi_call(MPI_Waitany);

        call_completed = true;
        break;
        //return MPI_SUCCESS;
      }
      reqPtrs[numNonnull++] = reqPtr;
    }
  }

  if (!call_completed && numNonnull == 0){
    spkt_abort_printf("MPI_Waitany: passed in all null requests, undefined behavior");
    call_completed = true;
    //return MPI_SUCCESS;
  }

  if(!call_completed) {
    //none of them are already done
    reqPtrs.resize(numNonnull);
    queue_->start_progress_loop(reqPtrs);

    numNonnull = 0;
    for (int i=0; i < count; ++i){
      MPI_Request req = array_of_requests[i];
      if (req != MPI_REQUEST_NULL){
        mpi_request* reqPtr = reqPtrs[numNonnull++];
        if (reqPtr->is_complete()){
          *indx = i;
          req_val = req;
          finalize_wait_request(reqPtr, &array_of_requests[i], status);
          finish_mpi_call(MPI_Waitany);
          call_completed = true;
          break;
          //return MPI_SUCCESS;
        }
      }
    }
  }

  if(!call_completed)
    spkt_throw_printf(sprockit::value_error,
                    "MPI_Waitany finished, but had no completed requests");

  #ifdef SSTMAC_OTF2_ENABLED
    if(otf2_enabled_ && otf2_initialized_) {
      if(status->count != 0) {
        otf2_writer_.mpi_waitany(comm_world()->rank(),
                                 call_start_time,
                                 (uint64_t)os_->now().usec(),
                                 req_val);
      } else {
        otf2_writer_.generic_call(comm_world()->rank(),
                                  call_start_time,
                                  (uint64_t)os_->now().usec(),
                                  "MPI_Waitany");
      }
    }
  #endif

  return MPI_SUCCESS;
}

int
mpi_api::waitsome(int incount, MPI_Request array_of_requests[],
                  int *outcount, int array_of_indices[], MPI_Status array_of_statuses[])
{
#ifdef SSTMAC_OTF2_ENABLED
  // Cache the vector before it is destroyed
  std::vector<MPI_Request> req_vect(array_of_requests, array_of_requests + incount);
  auto call_start_time = (uint64_t)os_->now().usec();
#endif

  start_mpi_call(MPI_Waitsome);
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request,
    "MPI_Waitsome(...)");
  int numComplete = 0;
  int numIncomplete = 0;
  std::vector<mpi_request*> reqPtrs(incount);
  for (int i=0; i < incount; ++i){
    MPI_Request req = array_of_requests[i];
    if (req != MPI_REQUEST_NULL){
      mpi_request* reqPtr = get_request(req);
      if (reqPtr->is_complete()){
        array_of_indices[numComplete++] = i;
        finalize_wait_request(reqPtr, &array_of_requests[i],
           ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i]);
      } else {
        reqPtrs[numIncomplete++] = reqPtr;
      }
    }
  }

  if (numComplete > 0){
    *outcount = numComplete;
    //return MPI_SUCCESS;
  } else {
    reqPtrs.resize(numIncomplete);

    queue_->start_progress_loop(reqPtrs);

    for (int i=0; i < incount; ++i){
      MPI_Request req = array_of_requests[i];
      if (req != MPI_REQUEST_NULL){
        mpi_request* reqPtr = get_request(req);
        if (reqPtr->is_complete()){
          array_of_indices[numComplete++] = i;
          finalize_wait_request(reqPtr, &array_of_requests[i],
             ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i]);
        }
      }
    }
    *outcount = numComplete;
    finish_mpi_call(MPI_Waitsome);
  }

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_ && otf2_initialized_) {
    otf2_writer_.mpi_waitsome(comm_world()->rank(),
                              call_start_time,
                              (uint64_t)os_->now().usec(),
                              req_vect.data(),
                              *outcount,
                              array_of_indices);
  }
#endif

  return MPI_SUCCESS;
}

}
