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

#include <sstmac/common/messages/sleep_message.h>
#include <sstmac/software/libraries/compute/lib_sleep.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

key::category lib_sleep::key_category("Sleep");

lib_sleep::lib_sleep(software_id id)
  : library()
{
  libname_ = "sleeplib" + id.to_string();
  key_cat_ = lib_sleep::key_category;
}

lib_sleep::lib_sleep(const std::string& id)
  : library()
{
  libname_ = id;
  key_cat_ = lib_sleep::key_category;
}

void
lib_sleep::sleep(timestamp time)
{
  SSTMACBacktrace("Sleep");
  if (supported()) {
    sst_message::ptr cmsg = new sleep_message(lib_name(), time);
    os_->execute_kernel(ami::COMP_SLEEP, cmsg);
  }
  else {
    spkt_throw(sprockit::value_error,
      "lib_sleep requires the presence of a processor model which can sleep");
  }
}

bool
lib_sleep::supported() const
{
  return os_->kernel_supported(ami::COMP_SLEEP);
}

}
} //end of namespace sstmac

