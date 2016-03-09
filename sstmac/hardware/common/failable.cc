#include <sstmac/hardware/common/failable.h>
#include <sstmac/hardware/common/messages/fail_message.h>
#include <sstmac/common/event_manager.h>

namespace sstmac {
  namespace hw {

void
failable::handle(sst_message* msg)
{
  if (failed_){
    handle_while_failed(msg);
  }
  else if (msg->type() == node_fail_message::FAILURE){
    //I fail!
    fail(msg);
  }
  else {
    handle_while_running(msg);
  }
}

void
failable::fail(sst_message* msg)
{
  if (failed_)
    return;

  failed_ = true;
  do_failure(msg);
  cancel_all_messages();
}

  }
}
