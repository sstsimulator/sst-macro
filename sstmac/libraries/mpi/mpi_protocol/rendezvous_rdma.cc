#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

rendezvous_rdma::~rendezvous_rdma()
{
}

void
rendezvous_rdma::send_header(mpi_queue* queue,
                             mpi_message* msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Send Header");
  //msg->set_needs_ack(false);  //we don't care about the nic ack for this
  msg->content_type(mpi_message::header);
  queue->post_header(msg);
}

void
rendezvous_rdma::incoming_header(mpi_queue* queue,
                               mpi_message* msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Handle Header");

  mpi_queue_recv_request* req = queue->find_pending_request(msg);
  if (req) {
    if (req->open_source()) {
      //we have to initialize the recv
      //event* ev = new_event(req, &mpi_queue::recvrequest::handle, msg);
      //init_system_recv(msg->payload_bytes(), queue, ev);
      req->handle(msg);
    }
    else {
      req->handle(msg);
    }
  }
  queue->notify_probes(msg);
}

void
rendezvous_rdma::incoming_payload(mpi_queue* queue,
                                mpi_message* msg)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Handle Payload");

  mpi_queue::pending_req_map::iterator it = queue->recv_needs_payload_.find(
        msg->unique_mpi_id());
  if (it == queue->recv_needs_payload_.end()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpi_queue[%s]: rendezvous_rdma::handle_payload: "
                     "data message without a matching ack on %s",
                     queue->id_string().c_str(),
                     msg->to_string().c_str());
  }
  mpi_queue_recv_request* recver = it->second;
  queue->recv_needs_payload_.erase(it);
  recver->handle(msg);
  delete msg;
}

bool
rendezvous_rdma::send_needs_completion_ack() const
{
  return true;
}

bool
rendezvous_rdma::send_needs_rendezvous_ack() const
{
  return false;
}

void
rendezvous_rdma::finish_recv_header(
  mpi_queue* queue,
  mpi_message* msg,
  mpi_queue_recv_request* req
)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Recv Header");

  msg->convert(mpi_message::HEADER_TO_RDMA_GET_REQ);
  msg->set_needs_ack(false);
  queue->recv_needs_payload_[msg->unique_mpi_id()] = req;
  queue->post_rdma(msg);
}

void
rendezvous_rdma::finish_recv_payload(
  mpi_queue* queue,
  mpi_message* msg,
  mpi_queue_recv_request* req
)
{
  SSTMACBacktrace("MPI Rendezvous Protocol: RDMA Recv Payload");
  queue->send_completion_ack(msg);
}


}
}

