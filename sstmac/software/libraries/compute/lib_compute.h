#ifndef sstmac_libraries_compute_LIBCOMPUTE_H
#define sstmac_libraries_compute_LIBCOMPUTE_H

#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/graphviz.h>
#include <sprockit/debug.h>

DeclareDebugSlot(lib_compute)

namespace sstmac {
namespace sw {

class lib_compute :
  public library
{  
 public:
  static key::category key_category;
  
 protected:
  lib_compute()
    : library() {
    key_cat_ = lib_compute::key_category;
  }

};

}
}

#endif // LIBCOMPUTE_H

