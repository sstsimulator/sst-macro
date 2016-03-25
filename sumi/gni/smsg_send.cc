#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::send_pending_smsg()
{
  if (pending_smsg_.empty())
      return;

  gni_debug("trying to send pending");

  std::list<pending_smsg_t*> complete;

  {
  std::list<pending_smsg_t*>::iterator it = pending_smsg_.begin(), end = pending_smsg_.end();
  while (it != end)
  {
      std::list<pending_smsg_t*>::iterator tmp = it++;
      pending_smsg_t* pending = *tmp;
      gni_return_t rc = GNI_SmsgSend(
                          tx_context_.ep_handles[pending->dst],
                          pending->header,
                          pending->header_length,
                          pending->payload,
                          pending->payload_length,
                          0);

      if (rc == GNI_RC_SUCCESS){
          gni_debug("Sending a pending SMSG payload to node %d on node %d \n", pending->dst, rank_);
          complete.push_back(pending);
          pending_smsg_.erase(tmp);
      }
  }
  }

  {
  std::list<pending_smsg_t*>::iterator it = complete.begin(), end = complete.end();
  while (it != end)
  {
    pending_smsg_t* pending = *it;
    delete_smsg_header(pending->header);
    delete pending;
    ++it;
  }
  }
}

void
gni_transport::smsg_send(int dst, const message::ptr &msg, header_type_t type)
{
  gni_debug("sending header of type %d to %d", rank_, type, dst);
  if (dst == rank_){
    gni_debug("sending self smsg %s", rank_, msg->to_string().c_str());
    handle(msg->clone_msg());
    return;
  }
  smsg_payload_header_t* header = new smsg_payload_header_t;
  gni_debug("allocating message buffer");
  char* buffer = allocate_message_buffer(msg, header->length);
  header->type = type;
  header->buffer = buffer;
  gni_debug("smsg send");
  smsg_send(dst, header, sizeof(smsg_payload_header_t), buffer, header->length);
}

void
gni_transport::do_smsg_send(int dst, const message::ptr &msg)
{
  smsg_send(dst, msg, PAYLOAD);
}

#define ALLOW_SMSG_SEND_FAIL false

void
gni_transport::smsg_send(
  int dst,
  void* header,
  uint32_t header_length,
  void* payload,
  uint32_t payload_length
)
{
  send_pending_smsg();

  uint32_t msg_id = allocate_smsg_id();

  gni_debug("completed send header on node %d for header %p msgid %d", rank_, header, msg_id);

  gni_ep_handle_t ep_handle = tx_context_.ep_handles[dst];
  gni_return_t rc = GNI_SmsgSend(
    ep_handle,
    header,
    header_length,
    payload,
    payload_length,
    msg_id);

  if (ALLOW_SMSG_SEND_FAIL && rc == GNI_RC_NOT_DONE){
    pending_smsg_t* pending = new pending_smsg_t;
    pending->dst = dst;
    pending->header = header;
    pending->header_length = header_length;
    pending->payload = payload;
    pending->payload_length = payload_length;
    pending->done = false;
    pending_smsg_.push_back(pending);
  }
  else if (rc == GNI_RC_SUCCESS){
    gni_debug("Sending a SMSG payload to node %d on node %d", dst, rank_);
    smsg_headers_[msg_id] = header;
  }
  else {
    gni_rc_error(rc, "gni smsg send to dst %d, header=%p length=%lu, payload=%p, length=%lu",
                 dst, header, header_length, payload, payload_length);
  }
}

}
