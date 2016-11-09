#ifndef LIBRARY_MESSAGE_H
#define LIBRARY_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>

namespace sstmac {

/**
 * @brief The library_interface class provides a castable type that
 * stores a library id (std::string) for mapping events to the
 * correct library in sstmac::sw::operating_system.
 */
class library_interface
{

 public:
  /**
   * @return Node-unique name of library to deliver event to
   */
  const std::string&
  lib_name() const {
    return lib_name_;
  }

  void
  set_lib_name(const std::string& name) {
    lib_name_ = name;
  }

  void
  serialize_order(serializer& ser);

 protected:
  library_interface(const std::string& libname) :
    lib_name_(libname) {
  }

  library_interface() {}

  void
  clone_into(library_interface* cln) const {
    cln->lib_name_ = lib_name_;
  }

 protected:
  std::string lib_name_;

};

}

#endif // LIBRARY_MESSAGE_H

