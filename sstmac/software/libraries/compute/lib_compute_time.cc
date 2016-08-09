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

#include <sstmac/software/libraries/compute/lib_compute_time.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/common/messages/sleep_event.h>

namespace sstmac {
namespace sw {

lib_compute_time::lib_compute_time(software_id id) :
  lib_compute_time("libcomputetime", id)
{
}

lib_compute_time::lib_compute_time(const char* prefix, software_id id) :
  lib_compute(prefix, id)
{
}

lib_compute_time::lib_compute_time(const std::string& name, software_id id) :
  lib_compute(name, id)
{
}

lib_compute_time::~lib_compute_time()
{
}

void
lib_compute_time::compute(timestamp time)
{
  SSTMACBacktrace("Compute Time");
  if (time.sec() < 0) {
    spkt_throw(sprockit::value_error,
              "lib_compute_time can't compute for less than zero time");
  }
  os_->compute(time);
}

}
} //end of namespace sstmac

