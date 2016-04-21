#ifndef FAIL_MESSAGE_H
#define FAIL_MESSAGE_H

#include <sstmac/common/sst_event.h>

namespace sstmac {
  namespace hw {

class fail_event :
 public event,
 public serializable_type<fail_event>
{
  ImplementSerializable(fail_event)

 public:
  bool
  is_failure() const {
    return true;
  }

  virtual void
  serialize_order(serializer &ser){}

  std::string
  to_string() const {
    return "fail message";
  }

};

  }
}

#endif // FAIL_MESSAGE_H
