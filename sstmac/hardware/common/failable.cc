#include <sstmac/hardware/common/failable.h>
#include <sstmac/hardware/common/fail_event.h>
#include <sstmac/common/event_manager.h>

DeclareSerializable(sstmac::hw::fail_event)

namespace sstmac {
  namespace hw {

void
failable::handle(event* ev)
{
  if (failed_){
    handle_while_failed(ev);
  }
  else if (ev->is_failure()){
    //I fail!
    fail(ev);
  }
  else {
    handle_while_running(ev);
  }
}

void
failable::fail(event* ev)
{
  if (failed_)
    return;

  failed_ = true;
  do_failure(ev);
  cancel_all_messages();
}

  }
}
