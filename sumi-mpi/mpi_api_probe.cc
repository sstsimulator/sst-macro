#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>

namespace sumi {

int
mpi_api::probe(int source, int tag, MPI_Comm comm, MPI_Status *status)
{
  start_mpi_call("MPI_Probe");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Probe(%s,%s,%s)",
    src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());

  mpi_comm* commPtr = get_comm(comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  queue_->probe(req, commPtr, source, tag);
  queue_->progress_loop(req);

  if (status != MPI_STATUS_IGNORE){
    *status = req->status();
  }

  delete req;
  return MPI_SUCCESS;
}

int
mpi_api::iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status)
{
  start_mpi_call("MPI_Iprobe");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Iprobe(%s,%s,%s)",
    src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());

  mpi_comm* commPtr = get_comm(comm);
  bool found = queue_->iprobe(commPtr, source, tag, status);
  if (found){
    *flag = 1;
  } else {
    if (iprobe_delay_us_){
      queue_->forward_progress(1e-6*iprobe_delay_us_);
      found = queue_->iprobe(commPtr, source, tag, status);
    }

    if (found) *flag = 1;
    else       *flag = 0;
  }

  return MPI_SUCCESS;
}


}
