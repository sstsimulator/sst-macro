#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::send_cq_poll()
{
  gni_cq_entry_t event_data;
  while (1)
  {
    tx_context_.lock();
    gni_return_t rc = GNI_CqGetEvent(tx_context_.cq_handle, &event_data);
    if (rc == GNI_RC_ERROR_RESOURCE){
      gni_rc_error(rc, "CqGetEvent");
    }
    if (rc != wunderbahr){
      tx_context_.unlock();
      return;
    }

    uint64_t type = GNI_CQ_GET_TYPE(event_data);
    uint64_t src = RANK(GNI_CQ_GET_INST_ID(event_data));
    gni_debug("got send of type %d for src %d", type, src);
    if (type == GNI_CQ_EVENT_TYPE_POST){
      //I have to hold the lock until the RDMA is processed
      rdma_done(src, event_data, tx_context_.cq_handle);
      tx_context_.unlock();
    } else if (type == GNI_CQ_EVENT_TYPE_SMSG){
      //I can release the lock right away
      tx_context_.unlock();
      uint64_t msg_id = GNI_CQ_GET_MSG_ID(event_data);
      smsg_send_done(src, msg_id);
    }
  }
}

}
