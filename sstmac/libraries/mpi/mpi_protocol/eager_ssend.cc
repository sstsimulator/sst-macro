
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sw {


void
eager_ssend::finish_recv_header(
  mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req
)
{
  spkt_throw_printf(sprockit::illformed_error,
        "eager_ssend protocol should never have recv-request handle a header");
}

void
eager_ssend::finish_recv_payload(
  mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req
)
{
  queue->send_completion_ack(msg);
}

bool
eager_ssend::send_needs_completion_ack() const
{
  return true;
}

bool
eager_ssend::send_needs_nic_ack() const
{
  return false;
}

bool
eager_ssend::send_needs_eager_ack() const
{
  return false;
}

bool
eager_ssend::send_needs_rendezvous_ack() const
{
  return false;
}

void
eager_ssend::incoming_payload(mpi_queue* queue,
                            const mpi_message::ptr& msg)
{
  mpi_queue_recv_request* req = queue->find_pending_request(msg);
  if (req) {
    req->handle(msg);
  }
  queue->notify_probes(msg);
  //SSTMAC_DEBUG << "mpi_queue[" << queue->id_string()
  //             << "]:: eager_ssend::handle_header done \n";
}

bool
eager_ssend::handshake_only() const
{
  return false;
}

void
eager_ssend::send_header(mpi_queue* queue,
                         const mpi_message::ptr& msg)
{
  msg->content_type(mpi_message::eager_payload);
  queue->post_header(msg);
  //SSTMAC_DEBUG << "mpi_queue[" << queue->id_string()
  //             << "]:: eager_ssend::ssend_header done \n";
}

void
eager_ssend::incoming_header(mpi_queue* queue,
                           const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
        "eage_ssend protocol handling header, but only payloads should arrive");
}

}
}

