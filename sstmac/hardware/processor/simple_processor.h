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

#ifndef SSTMAC_HARDWARE_PROCESSOR_SIMPLEPROCESSOR_H_INCLUDED
#define SSTMAC_HARDWARE_PROCESSOR_SIMPLEPROCESSOR_H_INCLUDED

#include <sstmac/hardware/processor/processor.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/messages/timed_event.h>

namespace sstmac {
namespace hw {


/**
 * A very simple processor model
 */
class simple_processor :
  public processor
{
 public:
  simple_processor(sprockit::sim_parameters* params,
                   memory_model* mem, node* nd) :
    processor(params, mem, nd) {}

  /// Goodbye.
  virtual ~simple_processor() {}

  void
  compute(event* ev, callback* cb) override;

};

}
} // end of namespace sstmac

#endif

