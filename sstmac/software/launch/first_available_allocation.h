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

#include <sstmac/software/launch/allocation_strategy.h>

namespace sstmac {
namespace sw {

class first_available_allocation : public allocation_strategy
{

 public:
  virtual
  ~first_available_allocation() throw ();

  void
  allocate(
    int nnode_requested,
    const node_set& available,
    node_set& allocation) const;

};


}
} // end of namespace sstmac


#endif // FIRSTAVAILABLEALLOCATION_H

