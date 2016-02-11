#ifndef SLEEP_MESSAGE_H
#define SLEEP_MESSAGE_H

#include <sstmac/common/messages/timed_message.h>
#include <sstmac/common/messages/library_message.h>

namespace sstmac {

class sleep_message :
  public library_interface,
  public timed_interface,
  public sst_message
{

 public:
  typedef sprockit::refcount_ptr<sleep_message> ptr;
  typedef sprockit::refcount_ptr<const sleep_message> const_ptr;

 public:
  sleep_message(const std::string& libname, const timestamp& t) :
    library_interface(libname),
    timed_interface(t) {
  }

  virtual void
  serialize_order(sprockit::serializer& ser);

  std::string
  to_string() const {
    return "sleep message";
  }


};

}

#endif // SLEEP_MESSAGE_H

