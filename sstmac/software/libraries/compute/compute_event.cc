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
#include <sstmac/software/process/thread.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/process/operating_system.h>

RegisterDebugSlot(compute_intensity,
    "print output related to compute intensity (flops,intops,bytes) of specific threads");

DeclareSerializable(sstmac::sw::compute_event)

namespace sstmac {
namespace sw {

compute_event::compute_event() :
  core_(sw::thread::no_core_affinity)
{
  ::memset(event_data_, 0, MAX_EVENTS * sizeof(uli));
}

std::string
compute_event::debug_string() const
{
  long ticks = event_value(time);
  if (ticks){
    timestamp t(ticks, timestamp::exact);
    return sprockit::printf("time=%8.4e ms", t.msec());
  } else {
    return sprockit::printf("nflops=%ld nintops=%ld nbytes=%ld",
        event_value(flop),
        event_value(intop),
        event_value(mem_sequential));
  }
}

long
compute_event::byte_length() const
{
  return event_data_[mem_sequential];
}

} // end namespace sw
} // end of namespace sstmac

