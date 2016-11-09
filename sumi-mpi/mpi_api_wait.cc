#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <cassert>

namespace sumi {

int
mpi_api::wait(MPI_Request *request, MPI_Status *status)
{
  start_mpi_call("MPI_Wait");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Wait(...)");
  return do_wait(request, status);
}

int
mpi_api::do_wait(MPI_Request *request, MPI_Status *status)
{
  MPI_Request req = *request;
  timestamp wait_start = now();
  if (req == MPI_REQUEST_NULL){
    return MPI_SUCCESS;
  }

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, 
    "   MPI_Wait_nonnull(%d)", req);

  mpi_request* reqPtr = get_request(req);
  if (!reqPtr->is_complete()){
   queue_->progress_loop(reqPtr);
  }

  finalize_wait_request(reqPtr, request, status, wait_start);

  return MPI_SUCCESS;
}

void
mpi_api::finalize_wait_request(mpi_request* reqPtr, MPI_Request* req,
                               MPI_Status* status, timestamp wait_start)
{
#if SSTMAC_COMM_SYNC_STATS
  if (sync_stats()){
    sync_stats()->collect(reqPtr->time_sent(),
                          reqPtr->time_arrived(),
                          now().sec(),
                          wait_start.sec());
  }
#endif
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
  start_mpi_call("MPI_Waitall");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, 
    "MPI_Waitall(%d,...)", count);
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  for (int i=0; i < count; ++i){
    MPI_Status* status = ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i];
    do_wait(&array_of_requests[i], status);
  }
  return MPI_SUCCESS;
}

int
mpi_api::waitany(int count, MPI_Request array_of_requests[], int *indx,
                 MPI_Status *status)
{
  sstmac::timestamp wait_start = now();
  start_mpi_call("MPI_Waitany");
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
        finalize_wait_request(reqPtr,
                              &array_of_requests[i],
                              status, wait_start);
        return MPI_SUCCESS;
      }
      reqPtrs[numNonnull++] = reqPtr;
    }
  }

  if (numNonnull == 0){
    printf("all null\n");
    return MPI_SUCCESS;
  }

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
        finalize_wait_request(reqPtr, &array_of_requests[i],
                              status, wait_start);
        return MPI_SUCCESS;
      }
    }
  }

  spkt_throw_printf(sprockit::value_error,
                    "MPI_Waitany finished, but had no completed requests");

  //must have all been null
  return MPI_SUCCESS;
}

int
mpi_api::waitsome(int incount, MPI_Request array_of_requests[],
                  int *outcount, int array_of_indices[], MPI_Status array_of_statuses[])
{
  start_mpi_call("MPI_Waitsome");
  timestamp wait_start = now();
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
           ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i],
           wait_start);
      } else {
        reqPtrs[numIncomplete++] = reqPtr;
      }
    }
  }

  if (numComplete > 0){
    *outcount = numComplete;
    return MPI_SUCCESS;
  }

  reqPtrs.resize(numIncomplete);

  queue_->start_progress_loop(reqPtrs);

  for (int i=0; i < incount; ++i){
    MPI_Request req = array_of_requests[i];
    if (req != MPI_REQUEST_NULL){
      mpi_request* reqPtr = get_request(req);
      if (reqPtr->is_complete()){
        array_of_indices[numComplete++] = i;
        finalize_wait_request(reqPtr, &array_of_requests[i],
           ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i],
           wait_start);
      }
    }
  }
  *outcount = numComplete;
  return MPI_SUCCESS;
}

}

