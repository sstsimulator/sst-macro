#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_send_request.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

bool
eager1_rdma::send_needs_completion_ack() const
{
  return false;
}

bool
eager1_rdma::send_needs_rendezvous_ack() const
{
  return false;
}

bool
eager1_rdma::send_needs_nic_ack() const
{
  return false;
}

bool
eager1_rdma::send_needs_eager_ack() const
{
  return true;
}

void
eager1_rdma::send_header(mpi_queue* queue,
                         const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Send RDMA Header");

  msg->content_type(mpi_message::header);
  msg->hw::network_message::set_type(hw::network_message::payload);
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
eager1_rdma::incoming_header(mpi_queue* queue,
                           const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Header");

  /** Don't involve any recv request yet.
      Receive doesn't need to be posted for this to go forward */

  if (msg->cat() == mpi_message::onesided) {
  }
  else {
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
      msg->set_protocol(mpi_protocol::eager1_rdma_doublecpy_protocol);
    }
    queue->notify_probes(msg);

    /** we need to send an RDMA GET request back to the original node */
    msg->convert(mpi_message::HEADER_TO_RDMA_GET_REQ);
    // this has already been received by mpi in sequence
    // make sure mpi still handles this since it won't match
    // the current sequence number
    msg->set_ignore_seqnum(true);
    queue->post_rdma(msg);
  }
}

void
eager1_rdma::finish_recv_header(mpi_queue* queue,
    const mpi_message::ptr& msg,
    mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::illformed_error,
       "eager1_rdma protocol should not have recv-request handle header");
}

void
eager1_rdma::finish_recv_payload(mpi_queue* queue,
    const mpi_message::ptr& msg, mpi_queue_recv_request* req)
{
}

eager1_rdma_singlecpy::~eager1_rdma_singlecpy()
{
}

void
eager1_rdma_singlecpy::incoming_payload(mpi_queue* queue,
                                      const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Paylod");

  if (msg->cat() == mpi_message::onesided) {
    queue->handle_put(msg);
  }
  else {
    mpi_queue_recv_request* req = queue->find_waiting_request(msg);
    if (!req){
      spkt_throw(sprockit::illformed_error,
        "running singlecpy eager1 protocol, but no matching request found");
    }
    //We have RDMA GOT direct into the buffer - just complete
    req->handle(msg);
  }
}

eager1_rdma_doublecpy::~eager1_rdma_doublecpy()
{
}

void
eager1_rdma_doublecpy::incoming_payload(mpi_queue* queue,
                                      const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 1 Protocol: Handle RDMA Paylod");
  //the recv request is expecting a user message - update the message to match
  mpi_queue_recv_request* req = queue->find_pending_request(msg,
                                     true); //if not found, add to need_recv
  if (req) { //just finish things off by copying buffers
    queue->buffered_recv(msg, req);
  }
}

bool
eager1_rdma::handshake_only() const
{
  //initial send is handshake only
  return true;
}

}
}

