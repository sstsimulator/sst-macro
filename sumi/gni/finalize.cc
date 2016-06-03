#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::finalize()
{
  transport::finalize();
  PMI_Barrier();

  finalize_smsg_buffer();
  finalize_end_points(tx_context_, false);
  finalize_cq(tx_context_.cq_handle);
  finalize_cq(smsg_rx_cq_);
  finalize_cq(rdma_rx_cq_);

  int ret;
  if ((ret = PMI2_Finalize()) != PMI_SUCCESS){
      gni_error("PMI Finalize");
  }

  delete[] peers_;
  delete[] tx_context_.ep_handles;
  delete[] nics_;
}

}
