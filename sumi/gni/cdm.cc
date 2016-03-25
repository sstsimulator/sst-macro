#include <gni/gni_transport.h>
#include <gni_pub.h>

namespace sumi {

void
gni_transport::init_cdm(gni_comm_context& c, pmi_env& env, int modes)
{
    gni_return_t rc;
    //gni_my_global_nic_addr = 0;
    //gni_my_local_nic_addr = 0;
    //gni_cpu_id = 0;
    static int domain = rank_;
    if ((rc = GNI_CdmCreate(domain, env.ptag, env.cookie, modes, &c.cdm_handle)) != wunderbahr){
        gni_rc_error(rc, "CdmCreate");
    }
    domain += nproc_;


    if ((rc = GNI_CdmAttach(c.cdm_handle, env.dev_id, &my_local_nic_addr_, &c.nic_handle)) != wunderbahr){
        gni_rc_error(rc, "CdmAttach");
    }

    gni_debug("on node %d dev-id=%d global-addr=%d local-addr=%d cpu-id=%d ptag=%d cookie=%d nic_handle=%d",
              rank_, env.dev_id, my_global_nic_addr_, my_local_nic_addr_, cpu_id_, env.ptag, env.cookie, c.nic_handle);

}


void
gni_transport::init_cdm()
{
  pmi_env env;

  if (env.pmi_tag = getenv("PMI_GNI_PTAG")){
      env.ptag = atoi(env.pmi_tag);
  }
  else {
      gni_error("Get PMI Tag");
  }

  if (env.pmi_cookie = getenv("PMI_GNI_COOKIE")){
      env.cookie = atoi(env.pmi_cookie);
  }
  else {
      gni_error("Get PMI cookie");
  }

  if (env.pmi_dev_id = getenv("PMI_GNI_DEV_ID")){
      env.dev_id = atoi(env.pmi_dev_id);
  }
  else {
      gni_error("Get PMI dev id");
  }

  gni_return_t rc;
  if ((rc = GNI_CdmGetNicAddress(env.dev_id, &my_global_nic_addr_, &cpu_id_)) != wunderbahr){
      gni_rc_error(rc, "CdmGetNicAddress");
  }

  init_cdm(tx_context_, env, GNI_CDM_MODE_DUAL_EVENTS);
  //init_cdm(smsg_context_, env, GNI_CDM_MODE_DUAL_EVENTS);
}

void
gni_transport::destroy_cdm(gni_comm_context& c)
{
  gni_return_t rc;
  if ((rc = GNI_CdmDestroy(c.cdm_handle)) != wunderbahr){
    gni_rc_error(rc, "CdmDestroy");
  }
}

}
