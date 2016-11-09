#include <sstmac/libraries/sumi/distributed_service.h>
#include <sstmac/libraries/sumi/sumi_transport.h>
#include <sumi/transport.h>
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::distributed_service)
MakeDebugSlot(distributed_service)
RegisterKeywords("services");

#define debug(...) debug_printf(sprockit::dbg::distributed_service, __VA_ARGS__)

namespace sstmac {

SpktRegister("distributed_service", sw::app, distributed_service_app);

distributed_service_app::distributed_service_app(sprockit::sim_parameters* params,
                    sw::software_id sid,
                    sw::operating_system* os) :
  app(params, sid, os)
{
  libname_ = params->get_param("libname");
}

void
distributed_service_app::skeleton_main()
{
  //need to pass libname twice - once for the factory, once for OS registration
  distributed_service* srv = distributed_service_factory::get_value(libname_, params_, libname_, sid(), os_);
  srv->init();
  debug("initialized distributed service %s on rank %d", libname_.c_str(), srv->rank());
  srv->barrier(0);
  srv->collective_block(sumi::collective::barrier, 0);
  debug("running distributed service %s on rank %d", libname_.c_str(), srv->rank());
  srv->run();
  srv->barrier(1);
  srv->collective_block(sumi::collective::barrier, 1);
  debug("finalizing distributed service %s on rank %d", libname_.c_str(), srv->rank());
  srv->finalize();
  delete srv;
}

sumi::message::ptr
distributed_service::poll_for_message(bool blocking)
{
  sumi::message::ptr msg = poll(blocking);
  if (msg && msg->class_type() == sumi::message::bcast){
    sumi::system_bcast_message::ptr smsg = ptr_safe_cast(sumi::system_bcast_message, msg);
    if (smsg->action() == sumi::system_bcast_message::shutdown){
      terminated_ = true;
      return sumi::message::ptr();
    }
  }
  return msg;
}


}
