#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {

void
eager0::configure_send_buffer(mpi_queue* queue, const mpi_message::ptr& msg,
                              void *buffer, mpi_type* type)
{
  if (buffer){
    void* eager_buf = fill_send_buffer(msg, buffer, type);
    msg->eager_buffer() = eager_buf;
  }
}

void
eager0::send_header(mpi_queue* queue,
                    const mpi_message::ptr& msg)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Send Header");
  msg->set_content_type(mpi_message::eager_payload);
  queue->post_header(msg,true/*do need an ack*/);
}

void
eager0::incoming_payload(mpi_queue* queue,
                        const mpi_message::ptr& msg)
{
  mpi_queue_recv_request* req = queue->pop_pending_request(msg);
  incoming_payload(queue, msg, req);
}

void
eager0::incoming_payload(mpi_queue *queue,
                  const mpi_message::ptr& msg,
                  mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI Eager 0 Protocol: Handle Header");
  if (req) {
    if (msg->local_buffer().ptr && req->recv_buffer_){
      msg->remote_buffer().ptr = req->recv_buffer_;
      msg->move_local_to_remote();
    }
    queue->finalize_recv(msg, req);
#if SSTMAC_COMM_SYNC_STATS
    msg->set_time_synced(queue->now());
#endif
  }
  else {
    queue->buffer_unexpected(msg);
  }

  queue->notify_probes(msg);

}


}

