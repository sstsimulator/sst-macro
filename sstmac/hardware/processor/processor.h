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

#ifndef SSTMAC_HARDWARE_PROCESSOR_PROCESSOR_H_INCLUDED
#define SSTMAC_HARDWARE_PROCESSOR_PROCESSOR_H_INCLUDED


#include <sstmac/common/timestamp.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/hardware/memory/memory_model_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(processor);

namespace sstmac {
namespace hw {

/**
 * An interface for processor models
 */
class processor
{

 public:
  virtual ~processor();

  static void
  delete_statics();

  virtual void
  compute(event* cev, callback* cb) = 0;

  int ncores() const {
    return ncores_;
  }

 protected:
  processor(sprockit::sim_parameters* params, memory_model* mem, node* nd);

 protected:
  double freq_;
  double mem_freq_;
  int ncores_;

  memory_model* mem_;
  node* node_;


};

DeclareFactory2InitParams(processor, memory_model*, node*);

}
} // end of namespace sstmac

#endif

