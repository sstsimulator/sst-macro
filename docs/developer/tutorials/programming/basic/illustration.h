#ifndef tutorials_programming_illustration_h
#define tutorials_programming_illustration_h

#include <sprockit/ptr_type.h>

namespace sstmac {
namespace tutorial {

/**
* @brief Basic class structure tutorial
* @class illustration
*/
class illustration :
  public sprockit::ptr_type
{
  /** Public typedefs */
 public:
  typedef sprockit::refcount_ptr<illustration> ptr;
  typedef sprockit::refcount_ptr<const illustration> const_ptr;


  /** Public functions and constructors */
 public:
  std::string
  to_string() const override {
    return "message class";
  }

  illustration(const std::string& msg)
    : message_(msg) {
  }

  void
  illustrate() {
    std::cout << message_ << std::endl;
  }

  std::string
  message() const {
    return message_;
  }

  void
  set_message(const std::string& msg) {
    message_ = msg;
  }

  /** Member variables */
 protected:
  std::string message_;

};

}
}

#endif

