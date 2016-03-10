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

#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sstmac/hardware/processor/simple_processor.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_message.h>
#include <sstmac/software/process/operating_system.h>
#include <iostream>

namespace sstmac {
namespace hw {

SpktRegister("simple", processor, simple_processor,
            "Basic processor that only does timed_message computes");

void
simple_processor::finalize_init()
{
  processor::finalize_init();
}

void
simple_processor::compute(sst_message* msg)
{
  sw::compute_message* cmsg = safe_cast(sw::compute_message, msg);
  //can only do timed compute
  timestamp time(cmsg->event_value(sw::compute_message::time), timestamp::exact);
  os_delayed_notify(time, cmsg);
}



}
} // end of namespace sstmac

