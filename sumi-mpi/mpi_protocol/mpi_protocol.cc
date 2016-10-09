#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>

namespace sumi {


mpi_protocol* mpi_protocol::eager0_protocol = new eager0;
mpi_protocol* mpi_protocol::eager1_singlecpy_protocol = new eager1_singlecpy;
mpi_protocol* mpi_protocol::eager1_doublecpy_protocol = new eager1_doublecpy;
mpi_protocol* mpi_protocol::rendezvous_protocol = new rendezvous_get;

static sprockit::need_delete_statics<mpi_protocol> del_statics;

mpi_protocol*
mpi_protocol::get_protocol_object(PROTOCOL_ID id) {
  switch (id) {
  case PROTOCOL_INVALID:
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi_protocol::get_protocol_object - invalid protocol");
    break;
  case EAGER0:
    return eager0_protocol;
  case EAGER1_SINGLECPY:
    return eager1_singlecpy_protocol;
  case EAGER1_DOUBLECPY:
    return eager1_doublecpy_protocol;
  case RENDEZVOUS_GET:
    return rendezvous_protocol;
  default:
    spkt_throw_printf(sprockit::value_error,
        "mpi_protocol: unknown id %d", id);
  }
}

void
mpi_protocol::delete_statics()
{
  delete eager0_protocol;
  delete eager1_singlecpy_protocol;
  delete eager1_doublecpy_protocol;
  delete rendezvous_protocol;
}

void
mpi_protocol::handle_nic_ack(mpi_queue* queue,
                             const mpi_message::ptr& msg)
{
  queue->complete_nic_ack(msg);
}

void
mpi_protocol::incoming_header(mpi_queue* queue,
  const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle header",
     to_string().c_str());
}

void
mpi_protocol::incoming_header(mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle header",
     to_string().c_str());
}

void
mpi_protocol::incoming_payload(mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle payload",
     to_string().c_str());
}

void
mpi_protocol::incoming_payload(mpi_queue* queue,
  const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle payload",
     to_string().c_str());
}

}

