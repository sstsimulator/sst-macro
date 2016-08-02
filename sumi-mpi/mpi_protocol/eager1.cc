#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {

void
eager1::configure_send_buffer(const mpi_message::ptr& msg, void *buffer)
{
  long length = msg->payload_bytes();
  void* eager_buf = new char[length];
  ::memcpy(eager_buf, buffer, length);
  msg->remote_buffer().ptr = eager_buf;
}

void
eager1::send_header(mpi_queue* queue,
                    const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Send RDMA Header");
  msg->set_content_type(mpi_message::header);
  queue->user_lib_mem()->copy(msg->byte_length());

  queue->post_header(msg, false/*the send is "done" - no need to ack*/);

  /** the send will have copied into a temp buffer so we can 'ack' the buffer for now */
  mpi_queue::send_needs_ack_t::iterator it, end =
    queue->send_needs_eager_ack_.end();
  for (it = queue->send_needs_eager_ack_.begin(); it != end; ++it) {
    if ((*it)->matches(msg)) {
      // Match.
      mpi_queue_send_request* sreq = *it;
      sreq->complete(msg);
      queue->send_needs_eager_ack_.erase(it);
      delete sreq;
      return;
    }
  }
  spkt_throw_printf(sprockit::illformed_error,
        "eager1 protocol could not find request to complete");
}

void
eager1::incoming_header(mpi_queue *queue,
                                  const mpi_message::ptr &msg)
{
  mpi_queue_recv_request* req =
    queue->pop_pending_request(msg, false);
  incoming_header(queue, msg, req);
}

void
eager1::incoming_header(mpi_queue* queue,
                        const mpi_message::ptr& msg,
                        mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Header");
  if (req) {
    //we can post an RDMA get request direct to the buffer
    //make sure to put the request back in, but alert it
    //that it should expect a data payload next time
    queue->waiting_message_.push_front(req);
    req->set_seqnum(msg->seqnum()); //associate the messages
    msg->local_buffer().ptr = req->buffer_;
  }
  else {
    msg->set_protocol(mpi_protocol::eager1_doublecpy_protocol);
  }
  queue->notify_probes(msg);

  // this has already been received by mpi in sequence
  // make sure mpi still handles this since it won't match
  // the current sequence number
  msg->set_ignore_seqnum(true);
  msg->set_content_type(mpi_message::data);
  // generate an ack ONLY on the recv end
  queue->post_rdma(msg, false, true);
}

void
eager1_singlecpy::incoming_payload(mpi_queue *queue,
                         const mpi_message::ptr &msg)
{
  mpi_queue_recv_request* req = queue->pop_waiting_request(msg);
  //guaranteed that req is posted before payload arrives
  incoming_payload(queue, msg, req);
}

void
eager1_doublecpy::incoming_payload(mpi_queue *queue,
                         const mpi_message::ptr &msg)
{
  mpi_queue_recv_request* req = queue->pop_pending_request(msg, true);
  //guaranteed that req is posted before payload arrives
  incoming_payload(queue, msg, req);
}

void
eager1_singlecpy::incoming_payload(mpi_queue *queue,
                                   const mpi_message::ptr &msg,
                                   mpi_queue_recv_request *req)
{
  if (!req){
    spkt_throw(sprockit::value_error,
               "eager1_singlecpy::incoming_payload: null recv request");
  }
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Payload");
  //already RDMA'd correctly - just finish
  queue->finalize_recv(msg, req);
}

void
eager1_doublecpy::incoming_payload(mpi_queue* queue,
                                   const mpi_message::ptr& msg,
                                   mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Payload");
  //We did not RDMA get directly into the buffer
  //finish the transfer
  if (req){
    if (req->buffer_){
      msg->local_buffer().ptr = req->buffer_;
      //bypass mpi-message - actually do the move
      msg->message::move_remote_to_local();
    }
    queue->finalize_recv(msg, req);
  }
}


}

