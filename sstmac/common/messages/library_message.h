#ifndef LIBRARY_MESSAGE_H
#define LIBRARY_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>

namespace sstmac {

class library_interface
{

 public:
  /**
   * Library name getter
   * @return Name of deliver-to library
   */
  std::string
  lib_name() const {
    return lib_name_;
  }

  /**
   * Library name setter
   * @param s Name of deliver-to library
   */
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

