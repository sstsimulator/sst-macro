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

#ifndef random_alloCATION_H
#define random_alloCATION_H

#include <sstmac/software/launch/node_allocator.h>

namespace sstmac {
namespace sw {

class random_allocation : public node_allocator
{
 public:
  random_allocation(sprockit::sim_parameters *params);

  std::string
  to_string() const override {
    return "random allocation";
  }

  virtual
  ~random_allocation() throw ();

  void
  allocate(
    int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set& allocation) const override;

 protected:
  RNG::UniformInteger* rng_;

};


}
} // end of namespace sstmac


#endif // random_alloCATION_H

