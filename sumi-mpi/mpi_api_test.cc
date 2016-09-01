#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/operating_system.h>

namespace sumi {

bool
mpi_api::test(MPI_Request *request, MPI_Status *status)
{
  start_mpi_call("MPI_Test");
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
  if (test(request, status)){
    *flag = 1;
  } else {
    *flag = 0;
  }

  return MPI_SUCCESS;
}

int
mpi_api::testall(int count, MPI_Request array_of_requests[], int *flag, MPI_Status array_of_statuses[])
{
  *flag = 1;
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  for (int i=0; i < count; ++i){
    MPI_Status* stat = ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i];
    if (!test(&array_of_requests[i], stat)){
      *flag = 0;
    }
  }
  return MPI_SUCCESS;
}

int
mpi_api::testany(int count, MPI_Request array_of_requests[], int *indx, int *flag, MPI_Status *status)
{
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
  return MPI_SUCCESS;
}

int
mpi_api::testsome(int incount, MPI_Request array_of_requests[], int *outcount, int array_of_indices[], MPI_Status array_of_statuses[])
{
  int numComplete = 0;
  bool ignore_status = array_of_statuses == MPI_STATUSES_IGNORE;
  for (int i=0; i < incount; ++i){
    MPI_Status* stat = ignore_status ? MPI_STATUS_IGNORE : &array_of_statuses[i];
    if (test(&array_of_requests[i], stat)){
      array_of_indices[numComplete++] = i;
    }
  }
  *outcount = numComplete;
  return MPI_SUCCESS;
}


}
