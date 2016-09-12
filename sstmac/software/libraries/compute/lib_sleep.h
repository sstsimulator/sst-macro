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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_SLEEP_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_SLEEP_H_INCLUDED

#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/common/sstmac_config.h>

namespace sstmac {
namespace sw {

class lib_sleep : public library
{
 public:
  static key::category key_category;

 public:
  virtual
  ~lib_sleep() {}

  lib_sleep(sprockit::sim_parameters* params, software_id id,
            operating_system* os);

  virtual void
  incoming_event(event *ev){
    library::incoming_event(ev);
  }

  void
  sleep(timestamp time);

};

}
} //end of namespace sstmac

#endif

