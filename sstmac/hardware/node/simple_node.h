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

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_SIMPLENODE_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_SIMPLENODE_H_INCLUDED

#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/processor/processor_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>

namespace sstmac {
namespace hw {

/**
 * A stand-in node.  This node type passes all requests directly to
 * the simulator and has no resource contention (infinite processors),
 */
class simple_node :
  public node
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  simple_node(
    SST::ComponentId_t id,
    SST::Params& params
  ) : node(id, params)
  { }
#endif

  virtual ~simple_node();

  virtual void
  finalize_init();

  void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  execute_kernel(ami::COMP_FUNC func,
                 sst_message* data);

  virtual void
  execute_kernel(ami::COMM_FUNC func,
                 sst_message* data);

  // ------- AMI functions --------- //
  virtual bool
  kernel_supported(ami::COMP_FUNC) const;

  virtual bool
  kernel_supported(ami::COMM_FUNC) const;

  // --------------------- //

#if !SSTMAC_INTEGRATED_SST_CORE
  virtual void
  set_event_manager(event_manager* man);
#endif

 protected:
  virtual bool
  try_comp_kernels(ami::COMP_FUNC func, sst_message* data);

};

DeclareIntegratedComponent(simple_node);

}
} // end of namespace sstmac

#endif

