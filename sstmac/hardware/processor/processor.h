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


#include <sstmac/common/event_handler.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/hardware/memory/memory_model_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/libraries/compute/compute_message_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(processor);

namespace sstmac {
namespace hw {

/**
 * An interface for processor models
 */
class processor :
  public event_handler,
  public sprockit::factory_type
{

 public:
  virtual ~processor();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  virtual void
  init_param1(memory_model* mem){
    mem_ = mem;
  }
  
  virtual void
  init_param2(node* nd){
    node_ = nd;
  }

  static void
  delete_statics();

  virtual void
  compute(sst_message* msg) = 0;
  
  void handle(sst_message*msg);

 protected:
  processor();

 protected:
  double freq_;
  double mem_freq_;

  memory_model* mem_;
  node* node_;
  
 protected:
  void os_delayed_notify(timestamp t, sw::compute_message* msg);
  void os_notify_now(sw::compute_message* msg);

};

DeclareFactory2InitParams(processor, memory_model*, node*);

}
} // end of namespace sstmac

#endif

