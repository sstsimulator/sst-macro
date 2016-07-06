#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/common/sstmac_env.h>
#include <sprockit/sim_parameters.h>

namespace sumi {

timestamp
mpi_queue::progress_loop(mpi_request* req)
{
  if (!req || req->is_complete()) {
    return os_->now();
  }

  mpi_queue_debug("entering progress loop");

  SSTMACBacktrace("MPI Queue Poll");
  while (!req->is_complete()) {
    mpi_queue_debug("blocking on progress loop");
    sumi::message::ptr msg = api_->blocking_poll();
    if (msg->class_type() == message::collective_done){
      handle_collective_done(msg);
    } else {
      mpi_message::ptr mpimsg = ptr_safe_cast(mpi_message, msg);
      mpi_queue_debug("continuing progress loop on incoming msg %s",
                      mpimsg->to_string().c_str());
      handle_incoming_message(mpimsg);
    }
  }
  mpi_queue_debug("finishing progress loop");

  return os_->now();
}

bool
mpi_queue::at_least_one_complete(const std::vector<mpi_request*>& req)
{
  mpi_queue_debug("checking if any of %d requests is done", (int)req.size());
  for (int i=0; i < (int) req.size(); ++i) {
    if (req[i] && req[i]->is_complete()) {
      mpi_queue_debug("request is done");
      os_->remove_blocker(req[i]->get_key());
      return true;
    }
  }
  return false;
}

void
mpi_queue::handle_collective_done(const sumi::message::ptr& msg)
{
  collective_done_message::ptr cmsg = ptr_safe_cast(collective_done_message, msg);
  mpi_comm* comm = safe_cast(mpi_comm, cmsg->dom());
  mpi_request* req = comm->get_request(cmsg->tag());
  collective_op_base* op = req->collective_data();
  api_->finish_collective(op);
  req->complete();
  delete op;
}

void
mpi_queue::start_progress_loop(const std::vector<mpi_request*>& req)
{
  mpi_queue_debug("starting progress loop");
  while (!at_least_one_complete(req)) {
    mpi_queue_debug("blocking on progress loop");
    sumi::message::ptr msg = api_->blocking_poll();
    if (msg->class_type() == message::collective_done){
      handle_collective_done(ptr_safe_cast(collective_done_message, msg));
    } else {
      mpi_message::ptr mpimsg = ptr_safe_cast(mpi_message, msg);
      mpi_queue_debug("continuing progress loop on incoming msg %s",
                      mpimsg->to_string().c_str());
      handle_incoming_message(mpimsg);
    }
  }
  mpi_queue_debug("finishing progress loop");
}

void
mpi_queue::start_progress_loop(
  const std::vector<mpi_request*>& req,
  timestamp timeout)
{
  start_progress_loop(req);
}

void
mpi_queue::finish_progress_loop(const std::vector<mpi_request*>& req)
{
}

void
mpi_queue::do_send(const mpi_message::ptr&mess)
{
  mess->protocol()->send_header(this, mess);
}

void
mpi_queue::do_recv(mpi_queue_recv_request*req)
{
  mpi_queue::start_recv(req);
}

void
mpi_queue::buffer_unexpected(const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Queue Buffer Unexpected Message");
  user_lib_mem_->copy(msg->payload_bytes());
}

void
mpi_queue::incoming_message(const mpi_message::ptr& message)
{
  handle_incoming_message(message);
}

void
mpi_queue::buffered_recv(const mpi_message::ptr& msg,
                                    mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI_buffered_recv");
  user_lib_mem_->copy(msg->payload_bytes());
  req->handle(msg);
}

void
mpi_queue::buffered_send(const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI_buffered_send");
  // we may be sending a header or a payload
  // regardless, we need to copy the actual payload
  user_lib_mem_->copy(msg->payload_bytes());
  post_header(msg, false/*the send is "done" - no need to ack*/);
}

void
mpi_queue::post_header(const mpi_message::ptr& msg, bool needs_ack)
{
  SSTMACBacktrace("MPI Queue Post Header");
  if (post_header_delay_.ticks_int64()) {
    user_lib_time_->compute(post_header_delay_);
  }
  mpi_comm* comm = api_->get_comm(msg->comm());
  int dst_world_rank = comm->peer_task(msg->dst_rank());
  msg->set_src_rank(comm->rank());
  api_->send_header(dst_world_rank, msg, needs_ack);
}

void
mpi_queue::post_rdma(const mpi_message::ptr& msg,
  bool needs_send_ack,
  bool needs_recv_ack)
{
  SSTMACBacktrace("MPI Queue Post RDMA Request");
  if (post_rdma_delay_.ticks_int64()) {
    user_lib_time_->compute(post_rdma_delay_);
  }
  mpi_comm* comm = api_->get_comm(msg->comm());
  int src_world_rank = comm->peer_task(msg->src_rank());
  api_->rdma_get(src_world_rank, msg, needs_send_ack, needs_recv_ack);
}

}

