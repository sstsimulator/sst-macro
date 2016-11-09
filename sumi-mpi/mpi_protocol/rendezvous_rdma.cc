#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_debug.h>

namespace sumi {


rendezvous_get::~rendezvous_get()
{
}

void
rendezvous_get::configure_send_buffer(const mpi_message::ptr& msg, void *buffer)
{
  msg->remote_buffer().ptr = buffer;
}

void
rendezvous_get::send_header(mpi_queue* queue,
                             const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Send Header");
  msg->set_content_type(mpi_message::header);
  queue->post_header(msg, false); //don't need the nic ack
}

void
rendezvous_get::incoming_header(mpi_queue* queue,
                               const mpi_message::ptr& msg)
{
  mpi_queue_recv_request* req = queue->pop_pending_request(msg);
  incoming_header(queue, msg, req);
}

void
rendezvous_get::incoming_header(mpi_queue *queue,
                                const mpi_message::ptr &msg,
                                mpi_queue_recv_request *req)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Handle Header");
#if SSTMAC_COMM_SYNC_STATS
  msg->set_time_sent(queue->now());
#endif
  if (req) {
    mpi_queue_action_debug(
      queue->api()->comm_world()->rank(),
      "found matching request for %s",
      msg->to_string().c_str());
    msg->set_needs_send_ack(false); //TODO do I need this?
    msg->set_content_type(mpi_message::data);
    msg->local_buffer().ptr = req->buffer_;
    queue->recv_needs_payload_[msg->unique_int()] = req;
    //generate both a send and recv ack
    queue->post_rdma(msg, true, true);
  } else {
    mpi_queue_action_debug(
      queue->api()->comm_world()->rank(),
      "no matching requests for %s",
      msg->to_string().c_str());
  }
  queue->notify_probes(msg);
}

void
rendezvous_get::incoming_payload(mpi_queue* queue,
                                const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Handle Payload");

  mpi_queue::pending_req_map::iterator it = queue->recv_needs_payload_.find(
        msg->unique_int());
  if (it == queue->recv_needs_payload_.end()) {
    if (queue->recv_needs_payload_.empty()){
      std::cerr << "No recv requests waiting" << std::endl;
    }
    for (auto& p : queue->recv_needs_payload_){
      mpi_message::id id = p.first;
      mpi_queue_recv_request* req = p.second;
      std::cerr << sprockit::printf("Waiting request: count=%d tag=%s comm=%s source=%s",
                    req->count_, 
                    queue->api()->tag_str(req->tag_).c_str(),
                    queue->api()->comm_str(req->comm_).c_str(),
                    queue->api()->src_str(req->source_).c_str()) 
                << std::endl;
    }
    int rank; queue->api_->comm_rank(MPI_COMM_WORLD, &rank);
    spkt_throw_printf(sprockit::illformed_error,
     "mpi_queue[%d]: rendezvous_get::handle_payload: "
     "queue %p data message %lu without a matching ack on %s",
      rank, queue, msg->unique_int(), msg->to_string().c_str());
  }
  mpi_queue_recv_request* recver = it->second;
  queue->recv_needs_payload_.erase(it);
  queue->finalize_recv(msg, recver);
}

}

