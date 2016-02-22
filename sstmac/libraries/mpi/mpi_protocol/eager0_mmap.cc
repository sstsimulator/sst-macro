#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

eager0_mmap::~eager0_mmap()
{
}

void
eager0_mmap::send_header(mpi_queue* queue,
                         const mpi_message::ptr& msg)
{
  //skip backtrace if service thread
  SSTMACBacktrace("MPI Eager 0 Protocol: Intranode Send Header");
  msg->content_type(mpi_message::eager_payload);
  queue->buffered_send(msg);
}

void
eager0_mmap::incoming_payload(mpi_queue* queue,
                           const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Intranode Handle Header");
  mpi_queue_recv_request* req = queue->find_pending_request(msg);
  if (req) {
    queue->buffered_recv(msg, req);
  }
  else {
    queue->buffer_unexpected(msg);
  }
  queue->notify_probes(msg);
}

}
}

