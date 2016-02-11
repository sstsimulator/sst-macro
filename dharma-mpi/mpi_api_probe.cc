#include <dharma-mpi/mpi_api.h>
#include <dharma-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sumi {

int
mpi_api::probe(int source, int tag, MPI_Comm comm, MPI_Status *status)
{
  SSTMACBacktrace("MPI_Probe");
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
  SSTMACBacktrace("MPI_Iprobe");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Iprobe(%s,%s,%s)",
    src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());

  mpi_comm* commPtr = get_comm(comm);
  bool found = queue_->iprobe(commPtr, source, tag, status);
  if (found){
    *flag = 1;
  } else {
    *flag = 0;
  }

  return MPI_SUCCESS;
}


}
}
