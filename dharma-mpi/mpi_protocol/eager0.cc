#include <dharma-mpi/mpi_protocol/mpi_protocol.h>
#include <dharma-mpi/mpi_queue/mpi_queue.h>
#include <dharma-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sumi {

void
eager0::finish_recv_header(
  mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req
)
{
  spkt_throw_printf(sprockit::illformed_error,
        "%s should never have recv-request handle a header",
        to_string().c_str());
}

void
eager0::finish_recv_payload(
  mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req
)
{
}

void
eager0::incoming_header(mpi_queue* queue,
                                const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
       "%s handling header, but only payloads should arrive",
       to_string().c_str());
}

void
eager0::send_header(mpi_queue* queue,
                         const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Send Header");
  queue->post_header(msg);
}

void
eager0::incoming_payload(mpi_queue* queue,
                           const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Handle Header");
  mpi_queue_recv_request* req = queue->find_pending_request(msg);
  if (req) {
    req->handle(msg);
  }
  else {
    queue->buffer_unexpected(msg);
  }
  queue->notify_probes(msg);
}

}
}

