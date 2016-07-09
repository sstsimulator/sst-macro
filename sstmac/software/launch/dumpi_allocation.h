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


#ifndef DUMPIALLOCATION_H
#define DUMPIALLOCATION_H

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/hostname_allocation.h>

namespace sstmac {
namespace sw {

class dumpi_allocation : public node_allocator
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  allocate(int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set& allocation) const;

  virtual
  ~dumpi_allocation() throw () {
  }

 protected:
  std::string metafile_;


};

}
}


#endif // DUMPIALLOCATION_H

