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

#ifndef SSTMAC_SOFTWARE_SERVICES_LAUNCH_ALLOCATION_CARTALLOCATION_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_LAUNCH_ALLOCATION_CARTALLOCATION_H_INCLUDED

#include <vector>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/allocation_strategy.h>

namespace sstmac {
namespace sw {

class cart_allocation :
  public allocation_strategy
{

 public:
  virtual void
  set_topology(hw::topology *top);

  void
  init_factory_params(sprockit::sim_parameters* params);

  virtual
  ~cart_allocation() throw () {}

  void
  allocate(int nnode, node_set &allocation);

 private:
  void
  insert(const std::vector<int>& coords, node_set& allocation);

  void
  allocate_dim(int dim, std::vector<int>& vec, node_set& allocation);

  std::vector<int> sizes_;
  std::vector<int> offsets_;

  hw::structured_topology* regtop_;
  bool auto_allocate_;

};

}
}

#endif

