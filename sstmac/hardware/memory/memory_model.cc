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

#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::memory_model);
RegisterDebugSlot(memory, "debug info related to memory accesses");

namespace sstmac {
namespace hw {

static sprockit::need_delete_statics<memory_model> need_del;

memory_model::memory_model(node* parent_node)
{
  parent_node_ = parent_node;
  nodeid_ = parent_node->addr();
  done_ = parent_node_;
  init_loc_id(event_loc_id(nodeid_));
}

node_id
memory_model::addr() const {
  return parent_node_->addr();
}

void
memory_model::delete_statics()
{
}

void
memory_model::init_factory_params(sprockit::sim_parameters *params)
{
}

memory_model::~memory_model()
{
}

}
} /* namespace sstmac */

