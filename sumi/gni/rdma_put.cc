#include <gni/gni_transport.h>
#include <cstring>

namespace sumi {

void
gni_transport::rdma_put_done(const message::ptr& msg)
{
  if (msg->needs_recv_ack() && !msg->has_transaction_id()){
    gni_debug("Rank %d software acking RDMA put to %d",
        rank_, msg->recver());
    smsg_send(msg->recver(), msg, RDMA_PUT_RECV_ACK);
  }
  if (msg->needs_send_ack()){
    message::ptr cln = msg->clone_ack();
    handle(cln);
  }
}

void
gni_transport::do_rdma_put(int dst, const message::ptr &msg)
{
  public_buffer& send_buf = msg->local_buffer();
  public_buffer& recv_buf = msg->remote_buffer();

  if (msg->byte_length() == 0){
    rdma_put_done(msg);
    return;
  }
  else if (dst == rank_){
    ::memcpy(recv_buf.ptr, send_buf.ptr, msg->byte_length());
    rdma_put_done(msg);
    return;
  }

  int tag = allocate_rdma_tag(msg);

  gni_debug("Rank %d RDMA put buffer %p into buffer %p on dst %d on tag %d",  
    rank_, (void*) send_buf, (void*) recv_buf, dst, tag);

  post_rdma(
    dst,
    msg->byte_length(),
    tag,
    send_buf.ptr,
    send_buf.mem_handle,
    recv_buf.ptr,
    recv_buf.mem_handle,
    GNI_POST_RDMA_PUT,
    GNI_CQMODE_GLOBAL_EVENT,
    //add any metadata for remote event acks
    PUT_ACK,
    msg->transaction_id());

  rdma_messages_[tag] = msg;
}

}



