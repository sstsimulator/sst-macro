#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

eager0_rdma::~eager0_rdma()
{
}

void
eager0_rdma::send_header(mpi_queue* queue,
                         mpi_message* msg)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Send Header");
  msg->content_type(mpi_message::eager_payload);
  queue->post_header(msg);
}

void
eager0_rdma::incoming_payload(mpi_queue* queue,
                           mpi_message* msg)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Handle Header");
  mpi_queue_recv_request* req = queue->find_pending_request(msg);
  if (req) {
    req->handle(msg);
    queue->notify_probes(msg);
    delete msg;
  }
  else {
    queue->buffer_unexpected(msg);
    queue->notify_probes(msg);
  }
}

}
}

