#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

rendezvous_socket::~rendezvous_socket()
{
}

bool
rendezvous_socket::send_needs_completion_ack() const
{
  return false;
}

bool
rendezvous_socket::send_needs_rendezvous_ack() const
{
  return true;
}

void
rendezvous_socket::send_header(mpi_queue* queue,
                               const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: Socket Send Header");

  msg->content_type(mpi_message::header);
  queue->post_header(msg);
}

void
rendezvous_socket::incoming_header(mpi_queue* queue,
                                 const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: Socket Handle Header");
  mpi_queue_recv_request* req = queue->find_pending_request(msg);
  if (req) {
    req->handle(msg);
  }

  queue->notify_probes(msg);

}

void
rendezvous_socket::incoming_payload(mpi_queue* queue,
                                  const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: Socket Handle Payload");

  mpi_queue::pending_req_map::iterator it = queue->recv_needs_payload_.find(
        msg->unique_mpi_id());
  if (it == queue->recv_needs_payload_.end()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpi_queue[%s]: rendezvous_socket::handle_payload: "
                     "data message without a matching ack on %s",
                     queue->id_string().c_str(),
                     msg->to_string().c_str());
  }
  mpi_queue_recv_request* recver = it->second;
  queue->recv_needs_payload_.erase(it);
  recver->handle(msg);
}

void
rendezvous_socket::finish_recv_payload(
  mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req
)
{
}

}
}


