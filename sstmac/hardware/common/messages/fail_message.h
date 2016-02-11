#ifndef FAIL_MESSAGE_H
#define FAIL_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>

namespace sstmac {
  namespace hw {

class fail_message :
 public sst_message
{
 public:
  typedef sprockit::refcount_ptr<fail_message> ptr;

  static sst_message::message_type_t FAILURE;

  std::string
  to_string() const {
    return "fail message";
  }

 protected:
  fail_message();

};

class node_fail_message :
 public fail_message
{
 public:
  typedef sprockit::refcount_ptr<node_fail_message> ptr;

};

  }
}

#endif // FAIL_MESSAGE_H
