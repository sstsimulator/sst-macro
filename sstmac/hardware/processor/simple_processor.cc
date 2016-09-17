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
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/process/operating_system.h>
#include <iostream>

namespace sstmac {
namespace hw {

SpktRegister("simple", processor, simple_processor,
            "Basic processor that only does timed_message computes");

void
simple_processor::compute(event* ev, callback* cb)
{
  sw::timed_compute_event* tev = safe_cast(sw::timed_compute_event, ev);
  node_->schedule_delay(tev->data(), cb);
}



}
} // end of namespace sstmac

