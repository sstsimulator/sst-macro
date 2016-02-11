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

#include <sstmac/backends/native/node_list.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace native {

//
// Get the given node in the list.
//
sstmac::hw::node*
nodelist::get(node_id id) const
{
  int64_t index = id;
  if(index < 0 || index >= int(nodes_.size())) {
    spkt_throw_printf(sprockit::value_error,
                     "nodelist::get: index %ld out of range", index);
  }
  return nodes_[index];
}

//
// Add the given node to the list.
//
node_id
nodelist::add(sstmac::hw::node*the_node)
{
  node_id new_id(this->size());
  nodes_.push_back(the_node);
  return new_id;
}

}
} // end of namespace sstmac

