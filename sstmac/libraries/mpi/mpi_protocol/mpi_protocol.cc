#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sw {

mpi_protocol* mpi_protocol::eager0_mmap_protocol = new eager0_mmap;
mpi_protocol* mpi_protocol::eager0_socket_protocol = new eager0_socket;
mpi_protocol* mpi_protocol::eager0_rdma_protocol = new eager0_rdma;
mpi_protocol* mpi_protocol::eager1_rdma_singlecpy_protocol = new eager1_rdma_singlecpy;
mpi_protocol* mpi_protocol::eager1_rdma_doublecpy_protocol = new eager1_rdma_doublecpy;
mpi_protocol* mpi_protocol::rendezvous_mmap_protocol = new rendezvous_mmap;
mpi_protocol* mpi_protocol::rendezvous_socket_protocol = new rendezvous_socket;
mpi_protocol* mpi_protocol::rendezvous_rdma_protocol = new rendezvous_rdma;
mpi_protocol* mpi_protocol::eager_ssend_protocol = new eager_ssend;
mpi_protocol* mpi_protocol::rendezvous_rma_protocol = new rendezvous_rma;

static sprockit::need_delete_statics<mpi_protocol> del_statics;

mpi_protocol*
mpi_protocol::get_protocol_object(PROTOCOL_ID id) {
  switch (id) {
  case PROTOCOL_INVALID:
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi_protocol::get_protocol_object - invalid protocol");
    break;
  case EAGER_SSEND:
    return eager_ssend_protocol;
  case EAGER0_MMAP:
    return eager0_mmap_protocol;
  case EAGER0_SOCKET:
    return eager0_socket_protocol;
  case EAGER0_RDMA:
    return eager0_rdma_protocol;
  case EAGER1_RDMA_SINGLECPY:
    return eager1_rdma_singlecpy_protocol;
  case EAGER1_RDMA_DOUBLECPY:
    return eager1_rdma_doublecpy_protocol;
  case RVOUS_MMAP:
    return rendezvous_mmap_protocol;
  case RVOUS_SOCKET:
    return rendezvous_socket_protocol;
  case RVOUS_RDMA:
    return rendezvous_rdma_protocol;
  case RVOUS_RMA:
    return rendezvous_rma_protocol;
  default:
    spkt_throw_printf(sprockit::value_error,
        "mpi_protocol: unknown id %d", id);
  }
}

void
mpi_protocol::delete_statics()
{
  delete eager0_mmap_protocol;
  delete eager0_socket_protocol;
  delete eager0_rdma_protocol;
  delete eager1_rdma_singlecpy_protocol;
  delete eager1_rdma_doublecpy_protocol;
  delete rendezvous_mmap_protocol;
  delete rendezvous_socket_protocol;
  delete rendezvous_rdma_protocol;
  delete eager_ssend_protocol;
  delete rendezvous_rma_protocol;
}

void
mpi_protocol::handle_nic_ack(mpi_queue* queue,
                             const mpi_message::ptr& msg)
{
  if (msg->count() >= 0) {
    queue->complete_nic_ack(msg);
  }
}

void
mpi_protocol::incoming_payload(mpi_queue* queue,
  const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle payload",
     to_string().c_str());
}

void
eager_protocol::configure_send_buffer(const mpi_message::ptr& msg, void *buffer)
{
  long length = msg->payload_bytes();
  void* eager_buf = new char[length];
  ::memcpy(eager_buf, buffer, length);
  msg->set_buffer(eager_buf, true); //is eager
}


}
}

