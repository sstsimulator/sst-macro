#ifndef SLEEP_MESSAGE_H
#define SLEEP_MESSAGE_H

#include <sstmac/common/messages/timed_event.h>

namespace sstmac {

class sleep_event :
  public timed_interface,
  public event
{
  NotSerializable(sleep_event)

 public:
  sleep_event(timestamp t) :
    timed_interface(t) {
  }

  std::string
  to_string() const {
    return "sleep message";
  }


};

}

#endif // SLEEP_MESSAGE_H

