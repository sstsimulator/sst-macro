#include <gni/gni_transport.h>
#include <iostream>

namespace sumi {

void
gni_transport::init_end_points(gni_comm_context& c)
{
  for (int i=0; i < nproc_; ++i){
    if (i==rank_)
        continue;

    uint32_t nic_addr = nics_[i].nic_addr;
    gni_debug("Binding endpoint %d with addr %d on node %d nic %d", i, nic_addr, rank_, c.nic_handle);
    gni_return_t rc;
    if ((rc = GNI_EpCreate(c.nic_handle, c.cq_handle, &c.ep_handles[i])) != wunderbahr){
        gni_rc_error(rc, "EpCreate");
    }

    if ((rc = GNI_EpBind(c.ep_handles[i], nic_addr, i)) != wunderbahr){
        gni_rc_error(rc, "EpBind");
    }
  }
}

void
gni_transport::finalize_end_points(gni_comm_context& c, bool wait)
{
  gni_return_t rc;
  for (int i=0; i < nproc_; ++i){
    if (i==rank_)
      continue;

    while (GNI_RC_NOT_DONE==GNI_EpUnbind(c.ep_handles[i])){
        if (!wait)
            break;
    }
    //validate_gni(rc, ep unbind);

    if ((rc = GNI_EpDestroy(c.ep_handles[i])) != wunderbahr){
        gni_rc_error(rc, "EpDestroy");
    }
  }
}

}
