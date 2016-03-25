#include <gni/gni_transport.h>
#include <cstring>

namespace sumi {

void
gni_transport::rdma_get_done(const message::ptr& msg)
{
  if (msg->needs_send_ack() && !msg->has_transaction_id()){
    gni_debug("Rank %d software acking RDMA get to %d",
        rank_, msg->sender());
    //have to send this to receiver to let them know
    msg->set_payload_type(message::rdma_get_ack);
    smsg_send(msg->sender(), msg, RDMA_GET_SEND_ACK);
    msg->set_payload_type(message::rdma_get);
  }
  if (msg->needs_recv_ack()){
    handle(msg);
  }
}

void
gni_transport::do_rdma_get(int src, const message::ptr &msg)
{
  public_buffer& send_buf = msg->remote_buffer();
  public_buffer& recv_buf = msg->local_buffer();

  if (msg->byte_length() == 0){
    rdma_get_done(msg);
    return;
  }
  else if (src == rank_){
    ::memcpy(recv_buf.ptr, send_buf.ptr, msg->byte_length());
    rdma_get_done(msg);
    return;
  }

  int tag = allocate_rdma_tag(msg);

  gni_debug("Rank %d RDMA get buffer %p into buffer %p from src %d on tag %d",  
    rank_,(void*) send_buf, (void*) recv_buf, src, tag);

  post_rdma(
    src,
    msg->byte_length(),
    tag,
    recv_buf.ptr,
    recv_buf.mem_handle,
    send_buf.ptr,
    send_buf.mem_handle,
    GNI_POST_RDMA_GET,
    GNI_CQMODE_GLOBAL_EVENT,
    GET_ACK,
    msg->transaction_id());

  rdma_messages_[tag] = msg;
}

}



