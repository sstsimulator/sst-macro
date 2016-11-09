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

#ifndef FIRSTAVAILABLEALLOCATION_H
#define FIRSTAVAILABLEALLOCATION_H

#include <sstmac/software/launch/node_allocator.h>

namespace sstmac {
namespace sw {

class first_available_allocation : public node_allocator
{
 public:
  first_available_allocation(sprockit::sim_parameters* params) :
    node_allocator(params)
  {
  }

  std::string
  to_string() const override {
    return "first available allocator";
  }

  virtual
  ~first_available_allocation() throw ();

  void
  allocate(
    int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set& allocation) const override;

};


}
} // end of namespace sstmac


#endif // FIRSTAVAILABLEALLOCATION_H

