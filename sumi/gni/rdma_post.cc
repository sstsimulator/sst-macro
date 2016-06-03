#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::post_rdma(
  int dst,
  size_t length,
  int tag,
  void *local_buffer,
  gni_mem_handle_t local_mem_handle,
  void *remote_buffer,
  gni_mem_handle_t remote_mem_handle,
  gni_post_type_t post_type,
  uint64_t cqmode,
  remote_rdma_event_t ev,
  int transaction_id
)
{
  gni_debug("Post RDMA on node %d to node %d of size %lu for tag %d for local region %p=(%lu,%lu) from remote region %p=(%lu,%lu)",
             rank_, dst,
             length, tag,
             local_buffer, local_mem_handle.qword1, local_mem_handle.qword2,
             remote_buffer, remote_mem_handle.qword1, remote_mem_handle.qword2);


  if (transaction_id >= 0){
    uint32_t tid = TID(transaction_id,ev,rank_);
    gni_debug("setting RDMA transaction ID %d for dst %d -> %u\n", transaction_id, dst, tid);
    gni_return rc = GNI_EpSetEventData(tx_context_.ep_handles[dst], dst, tid);
    if (rc != wunderbahr)
        gni_rc_error(rc, "SetEventData");
    cqmode |= GNI_CQMODE_REMOTE_EVENT;
  }

  gni_post_descriptor_t* pd = new gni_post_descriptor;
  pd->local_addr = (uint64_t) local_buffer;
  pd->local_mem_hndl = local_mem_handle;
  pd->remote_addr = (uint64_t) remote_buffer;
  pd->remote_mem_hndl = remote_mem_handle;
  pd->dlvr_mode = dlvr_mode_; //default is GNI_DLVMODE_IN_ORDER;
  pd->rdma_mode = rdma_mode_; //default is GNI_RDMAMODE_PHYS_ADDR;
  pd->cq_mode = cqmode;
  pd->sync_flag_value = 0;
  pd->sync_flag_addr = 0;
  pd->length = length;
  pd->src_cq_hndl = 0;//src_cq_handle;
  pd->type = post_type;
  pd->first_operand = (uint64_t) tag;
  pd->second_operand = (uint64_t) rank_; //to identify the instigator
  gni_return_t rc = GNI_PostRdma(tx_context_.ep_handles[dst], pd);
  if (rc != wunderbahr){
      gni_rc_error(rc, "PostRdma");
  }

}

}
