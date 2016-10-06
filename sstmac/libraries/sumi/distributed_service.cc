#include <sstmac/libraries/sumi/distributed_service.h>
#include <sstmac/libraries/sumi/sumi_transport.h>
#include <sumi/transport.h>

namespace sstmac
{

void
distributed_service::skeleton_main()
{
  sumi_transport* tport = get_api<sumi_transport>();
  tport->init();
  tport->barrier(0);
  tport->collective_block(sumi::collective::barrier, 0);
  run(tport);
  tport->barrier(1);
  tport->collective_block(sumi::collective::barrier, 1);
  tport->finalize();
}

sumi::message::ptr
distributed_service::busy_loop(sumi::transport* tport)
{
  sumi::message::ptr msg = tport->blocking_poll();
  switch (msg->class_type()){
    case sumi::message::bcast:
    {
      sumi::system_bcast_message::ptr smsg = ptr_safe_cast(sumi::system_bcast_message, msg);
      if (smsg->action() == sumi::system_bcast_message::shutdown){
        return sumi::message::ptr();
      }
    }
    default:
      return msg;
  }
}


}
