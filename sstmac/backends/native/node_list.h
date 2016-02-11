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

#ifndef SSTMAC_BACKENDS_NATIVE_NODELIST_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_NODELIST_H_INCLUDED

#include <sstmac/hardware/node/node.h>

#include <vector>

namespace sstmac  {
namespace native {


/**
 * A shared mapping from nodeid indices onto node pointers.
 */
class nodelist {

 public:
  virtual std::string
  to_string() const {
    return "node list";
  }

  /// The number of nodes in the list.
  int64_t
  size() const {
    return nodes_.size();
  }

  /// Get the given node in the list.
  sstmac::hw::node*
  get(node_id id) const;

  /// Add the given number of nodes to the list using the
  /// given object as a template.
  /// \return the index of the new node.
  node_id
  add(sstmac::hw::node* the_node);

 private:
  /// My nodes.
  std::vector<sstmac::hw::node*> nodes_;
};

}
} // end of namespace sstmac

#endif

