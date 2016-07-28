/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_LIBRARIES_LIBRARY_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_LIBRARY_H_INCLUDED

#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/event_location.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/libraries/library_fwd.h>
#include <sprockit/sim_parameters_fwd.h>


namespace sstmac {
namespace sw {

class library  {
  
 public:
  virtual void
  unregister_all_libs();

  virtual std::string
  to_string() const {
    return libname_;
  }

  virtual std::string
  lib_name() const {
    return libname_;
  }

  virtual void
  init_os(operating_system* os);

  virtual void
  consume_params(sprockit::sim_parameters* params);

  virtual void
  incoming_event(event* ev) = 0;

  operating_system*
  os() const {
    return os_;
  }

  software_id
  sid() const {
    return sid_;
  }

  virtual ~library();

 protected:
  library(const std::string& libname, software_id sid) :
    sid_(sid), libname_(libname), os_(0)
  {
  }

  library(const char* prefix, software_id sid) :
    sid_(sid), libname_(sprockit::printf("%s%s", prefix, sid.to_string().c_str())), os_(0)
  {
  }

  /**
   * This function is provided so that libraries can instantiate, register, and use other libraries.
   * @param lib the library to register
   */
  void
  register_lib(library* lib);

  /**
   * This is usually convenient to define to check if the node was set up correctly to support
   * what this library wants to do. This function is not currently a strict part of an interface.
   * @return true if the hardware supports this libraries functions
   */
  virtual bool
  supported() const {
    return true;
  }

 protected:
  operating_system* os_;
  key::category key_cat_;
  software_id sid_;

 private:
  std::string libname_;

};


}
} //end of namespace sstmac

#endif

