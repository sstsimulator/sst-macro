#include <sstmac/software/libraries/unblock_event.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

unblock_event::unblock_event(operating_system *os, key *k)
  : event(os->event_location(), os->event_location()),
  os_(os), key_(k)
{
}


void
unblock_event::execute()
{
  os_->unblock(key_);
}

timeout_event::timeout_event(operating_system* os, key* k) :
  event(os->event_location(), os->event_location()),
  os_(os), key_(k)
{
}


void
timeout_event::execute()
{
  if (key_->still_blocked()){
    key_->set_timed_out();
    os_->unblock(key_);
    delete key_;
  } else {
    //already fired, no timeout
    //I have to delete the key - no one else knows anything about it
    delete key_;
  }
}


}
}

