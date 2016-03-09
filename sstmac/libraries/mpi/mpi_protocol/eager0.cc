#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>

namespace sstmac {
namespace sw {


void
eager0_protocol::finish_recv_header(
  mpi_queue* queue,
  mpi_message* msg,
  mpi_queue_recv_request* req
)
{
  spkt_throw_printf(sprockit::illformed_error,
        "%s should never have recv-request handle a header",
        to_string().c_str());
}

void
eager0_protocol::finish_recv_payload(
  mpi_queue* queue,
  mpi_message* msg,
  mpi_queue_recv_request* req
)
{
}

bool
eager0_protocol::send_needs_eager_ack() const
{
  return false;
}

bool
eager0_protocol::send_needs_rendezvous_ack() const
{
  return false;
}

bool
eager0_protocol::send_needs_completion_ack() const
{
  return false;
}

bool
eager0_protocol::send_needs_nic_ack() const
{
  return true;
}

void
eager0_protocol::incoming_header(mpi_queue* queue,
                                mpi_message* msg)
{
  spkt_throw_printf(sprockit::illformed_error,
       "%s handling header, but only payloads should arrive",
       to_string().c_str());
}

bool
eager0_protocol::handshake_only() const
{
  return false;
}

}
}

