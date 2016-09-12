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
  lib_compute(sprockit::sim_parameters* params,
              const std::string& libname, software_id sid,
              operating_system* os)
    : library(libname, sid, os) {
    key_cat_ = lib_compute::key_category;
  }

  lib_compute(sprockit::sim_parameters* params,
              const char* name, software_id sid,
              operating_system* os)
    : library(name, sid, os)
  {
     key_cat_ = lib_compute::key_category;
  }

};

}
}

#endif // LIBCOMPUTE_H

