#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sw {

void
rendezvous_protocol::finish_recv_header(
  mpi_queue* queue,
  mpi_message* msg,
  mpi_queue_recv_request* req
)
{
  //ack here
  queue->recv_needs_payload_[msg->unique_mpi_id()] = req;
  queue->send_rendezvous_ack(msg);
}

bool
rendezvous_protocol::send_needs_eager_ack() const
{
  return false;
}

bool
rendezvous_protocol::send_needs_nic_ack() const
{
  return false;
}

bool
rendezvous_protocol::handshake_only() const
{
  return true;
}

}
}

