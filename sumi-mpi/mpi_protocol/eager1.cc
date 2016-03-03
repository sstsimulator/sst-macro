#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {


void
eager1::send_header(mpi_queue* queue,
                    const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Send RDMA Header");
  msg->set_content_type(mpi_message::header);
  queue->buffered_send(msg);

  /** the send will have copied into a temp buffer so we can 'ack' the buffer for now */
  mpi_queue::send_needs_ack_t::iterator it, end =
    queue->send_needs_eager_ack_.end();
  for (it = queue->send_needs_eager_ack_.begin(); it != end; ++it) {
    //horrible hack for now
    if ((*it)->matches(msg)) {
      // Match.
      mpi_queue_send_request* sreq = *it;
      sreq->complete(msg);
      /** we can complete the send request and allow
       the program to continue, but we have to wait
       for buffer resources to be completed by the rdma get */
      sreq->wait_for_buffer();
      return;
    }
  }
  spkt_throw_printf(sprockit::illformed_error,
        "eager1 protocol could not find request to complete");
}

void
eager1::incoming_header(mpi_queue* queue,
                           const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Header");

  /** Don't involve any recv request yet.
      Receive doesn't need to be posted for this to go forward */

  mpi_queue_recv_request* req =
    queue->find_pending_request(msg, false);
  if (req) {
    //we can post an RDMA get request direct to the buffer
    //make sure to put the request back in, but alert it
    //that it should expect a data payload next time
    queue->waiting_message_.push_front(req);
    req->set_seqnum(msg->seqnum()); //associate the messages
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
eager1::finish_recv_header(mpi_queue* queue,
    const mpi_message::ptr& msg,
    mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::illformed_error,
       "eager1_rdma protocol should not have recv-request handle header");
}

void
eager1::finish_recv_payload(mpi_queue* queue,
    const mpi_message::ptr& msg, mpi_queue_recv_request* req)
{
}

eager1_singlecpy::~eager1_singlecpy()
{
}

void
eager1_singlecpy::incoming_payload(mpi_queue* queue,
                                      const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Payload");
  mpi_queue_recv_request* req = queue->find_waiting_request(msg);
  if (!req){
    spkt_throw(sprockit::illformed_error,
      "running singlecpy eager1 protocol, but no matching request found");
  }
  //We have RDMA GOT direct into the buffer - just complete
  req->handle(msg);
}

eager1_doublecpy::~eager1_doublecpy()
{
}

void
eager1_doublecpy::incoming_payload(mpi_queue* queue,
                                   const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Payload");
  //the recv request is expecting a user message - update the message to match
  mpi_queue_recv_request* req = queue->find_pending_request(msg,
                                     true); //if not found, add to need_recv
  if (req) { //just finish things off by copying buffers
    queue->buffered_recv(msg, req);
  }
}

}

