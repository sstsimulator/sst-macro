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

#ifndef SSTMAC_SOFTWARE_SERVICES_SERVICE_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_SERVICE_H_INCLUDED

#include <sstmac/software/libraries/library.h>
#include <sprockit/factories/factory.h>

#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

namespace sstmac {
namespace sw {

class service :
  public library
{

 public:
  virtual std::string
  to_string() const {
    return "service";
  }

  virtual void
  start() {
  }

  virtual void
  incoming_event(event* ev) = 0;

 protected:
  service(const std::string& libname, software_id sid) :
    library(libname, sid)
  {}

  service(const char* prefix, software_id sid) :
    library(prefix, sid)
  {}

  virtual
  ~service(){}


};

}
} //end of namespace sstmac

#endif

