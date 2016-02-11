#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/rma/mpi_queue_get_request.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

void
rendezvous_rma::send_header(mpi_queue* queue,
                            const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RMA Send Header", queue->is_service_thread());
  //msg->set_needs_ack(false);  //we don't care about the nic ack for this
  msg->content_type(mpi_message::header);
  queue->post_header(msg);
}

void
rendezvous_rma::incoming_header(mpi_queue* queue,
                              const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RMA Handle Header");

  mpi_rma_message::ptr rmsg = ptr_safe_cast(mpi_rma_message, msg);

  if (queue->windows_.find(rmsg->winid()) == queue->windows_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
                     "rendezvous_rma::handle_header - window %d not stored in mpi_queue",
                     rmsg->winid());
  }

  spkt_throw(sprockit::unimplemented_error,
    "rendezvous rma");

#if 0
  mpi_window* win = queue->windows_[rmsg->winid()];

  //  if (win->current_epoch() == rmsg->epoch())
  //  {

  if (rmsg->get_rmatype() == mpi_rma_message::get) {
    mpi_rma_message::op_info inf = rmsg->get_opinfo();
    payload::const_ptr load = win->get_data(inf);

    rmsg->nic_event(sst_message::RDMA_GET_REQ_TO_RSP);
    rmsg->set_content(load);
    rmsg->set_needs_ack(false);
    queue->post_rdma(rmsg);
  }
  else if (rmsg->get_rmatype() == mpi_rma_message::put) {
    mpi_rma_message::op_info inf = rmsg->get_opinfo();
    win->put_data(inf, rmsg->content());

    rmsg->nic_event(sst_message::RDMA_PUT_REQ_TO_RSP);
    rmsg->set_content(payload::const_ptr());
    rmsg->set_needs_ack(false);
    queue->post_rdma(rmsg);
  }
  else if (rmsg->get_rmatype() == mpi_rma_message::acc) {
    mpi_rma_message::op_info inf = rmsg->get_opinfo();
    win->acc_data(inf, rmsg->content(), msg->type());

    rmsg->nic_event(sst_message::RDMA_PUT_REQ_TO_RSP);
    rmsg->set_content(payload::const_ptr());
    rmsg->set_needs_ack(false);
    queue->post_rdma(rmsg);
  }
#endif

  // }
  //  else
  //  {
  //    queue->headers_waiting_epoch_[rmsg->epoch()].push(msg);
  //  }

}

void
rendezvous_rma::incoming_payload(mpi_queue* queue,
                               const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RMA Handle Payload", queue->is_service_thread());

  mpi_queue::get_requests_t::iterator it =
    queue->gets_waiting_for_data_.find(msg->unique_mpi_id());
  if (it == queue->gets_waiting_for_data_.end()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpi_queue[%s]: rendezvous_rma::handle_payload: "
                     "data message without a matching ack on %s",
                     queue->id_string().c_str(),
                     msg->to_string().c_str());
  }
  mpi_queue_get_request* recver = it->second;
  queue->gets_waiting_for_data_.erase(it);
  recver->handle(msg);
}

bool
rendezvous_rma::send_needs_completion_ack() const
{
  return false;
}

bool
rendezvous_rma::send_needs_rendezvous_ack() const
{
  return false;
}

void
rendezvous_rma::finish_recv_header(mpi_queue* queue,
                            const mpi_message::ptr& msg, mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::spkt_error,
       "rendezvous_rma::recv_header - calling this function doesn't make sense");
}

void
rendezvous_rma::finish_recv_payload(mpi_queue* queue,
                             const mpi_message::ptr& msg, mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::spkt_error,
        "rendezvous_rma::recv_payload - calling this function doesn't make sense");

}

}
}

