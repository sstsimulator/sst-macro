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
#include <sstmac/software/libraries/compute/compute_message.h>
#include <sstmac/common/messages/sleep_message.h>

namespace sstmac {
namespace sw {

lib_compute_time::lib_compute_time(software_id id)
{
  libname_ = "computelib" + id.to_string();
}

lib_compute_time::lib_compute_time(const std::string& id)
{
  libname_ = id;
}

lib_compute_time*
lib_compute_time::construct(software_id id)
{
  return new lib_compute_time(id);
}

lib_compute_time*
lib_compute_time::construct(const std::string& id)
{
  return new lib_compute_time(id);
}

lib_compute_time::~lib_compute_time()
{
}

bool
lib_compute_time::supported() const
{
  return os_->kernel_supported(ami::COMP_TIME);
}

void
lib_compute_time::compute(timestamp time)
{
  SSTMACBacktrace("Compute Time");
  if (supported()) {
    if (time.sec() < 0) {
      spkt_throw(sprockit::value_error,
                "lib_compute_time can't compute for less than zero time");
    }
    compute_event* cmsg = new compute_event;
    cmsg->set_event_time(time);
    os_->execute_kernel(ami::COMP_TIME, cmsg);

    debug_printf(sprockit::dbg::lib_compute,
        "lib_compute_time: %p finishing compute kernel for delta_t=%12.8e sec",
        this, time.sec());

    debug_printf(sprockit::dbg::compute_intensity,
      "Node %d: finishing compute %s",
      int(os_->my_addr()),
      cmsg->debug_string().c_str());
    delete cmsg;
  }
  else {
    spkt_throw_printf(sprockit::value_error,
       "lib_compute_time::compute: requires processor model which can compute time blocks");
  }


}

}
} //end of namespace sstmac

