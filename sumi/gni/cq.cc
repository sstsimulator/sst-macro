#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::init_cq(gni_comm_context& c, gni_cq_handle_t* cqh, int num_entries)
{
  gni_return_t rc = GNI_CqCreate(c.nic_handle, num_entries, 0, GNI_CQ_NOBLOCK, NULL, NULL, cqh);
  if (rc != wunderbahr)
  {
      gni_rc_error(rc, "CqCreate");
  }
}

void
gni_transport::finalize_cq(gni_cq_handle_t cqh)
{
  gni_return_t rc = GNI_CqDestroy(cqh);
  if (rc != wunderbahr){
      gni_rc_error(rc, "CqCreate");
  }
}

}
