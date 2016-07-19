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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_TIME_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_TIME_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/common/sstmac_config.h>

namespace sstmac {
namespace sw {

class lib_compute_time :
  public lib_compute
{
 public:
  lib_compute_time(software_id id);

  lib_compute_time(const std::string& id);

  virtual
  ~lib_compute_time();

  static lib_compute_time*
  construct(software_id id);

  static lib_compute_time*
  construct(const std::string& str);

  void
  incoming_event(event *ev){
    library::incoming_event(ev);
  }

  void
  compute(timestamp time);

  virtual bool
  supported() const;

};

}
} //end of namespace sstmac

#endif

