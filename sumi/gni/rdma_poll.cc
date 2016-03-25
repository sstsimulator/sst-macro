#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::rdma_done(int src, gni_cq_entry_t& event_data, gni_cq_handle_t cqh)
{
  gni_post_descriptor_t *pd;
  gni_return_t rc = GNI_GetCompleted(cqh, event_data, &pd);
  if (rc != wunderbahr){
      gni_rc_error(rc, "GetCompleted on RDMA cq");
  }

  uint64_t post_type = pd->type;
  int tag = pd->first_operand;
  uint64_t type = pd->type;
  uint64_t instigator = pd->second_operand;

  gni_debug("Got completed RDMA on node %d for tag %d for partner %d", rank_, tag, src);
  if (tag == ping_response_tag){
    message::ptr ping_msg = new message;
    int* status_ptr = (int*) pd->local_addr;
    int status = *status_ptr;
    ping_msg->set_sender(src);
    ping_msg->set_recver(rank_);
    ping_msg->set_class_type(message::ping);
    if (status == i_am_dead){
      //oh no! he's dead!
      ping_msg->set_payload_type(message::rdma_get_nack);
    } else {
      ping_msg->set_payload_type(message::rdma_get);
    }
    handle(ping_msg);
    free_ping_buffer(status_ptr);
  } else {
    message::ptr msg = rdma_messages_[tag];
    rdma_messages_.erase(tag);
    if (type == GNI_POST_RDMA_GET){
      rdma_get_done(msg);
    } else if (type == GNI_POST_RDMA_PUT){
      rdma_put_done(msg);
    }
  }
  delete pd;
}

void
gni_transport::rdma_poll()
{
  /** Check the RDMA cq */
  gni_cq_entry_t event_data;
  gni_return_t rc = GNI_CqGetEvent(rdma_rx_cq_, &event_data);
  if (rc != wunderbahr)
    return;

  uint32_t inst_id = gni_cq_get_inst_id(event_data);
  int src = RANK(inst_id);
  remote_rdma_event_t ev = (remote_rdma_event_t) EVENT(inst_id);
  int id = ID(inst_id);
  gni_debug("Rank %d got transaction ack %d from %d for event %d -> %u",
    rank_, id, src, ev, inst_id);
  message::ptr header = finish_transaction(id);
  switch (ev)
  {
  case GET_ACK:
    //this event is an ack notifying me that my data went out
    header->set_payload_type(message::rdma_get_ack);
    break;
  case PUT_ACK:
    //this event is the actual payload arriving
    header->reverse(); //wrong directionality
    header->set_payload_type(message::rdma_put);
    break;
  default:
    spkt_throw_printf(sprockit::value_error,
      "gni_transpot::rdma_poll: invalid event %d", ev);
  }
  handle(header);
}


}

