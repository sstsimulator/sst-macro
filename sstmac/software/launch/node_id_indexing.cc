

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



#include <fstream>
#include <sstream>

#include <sstmac/software/launch/node_id_indexing.h>
#include <sstmac/software/launch/node_id_allocation.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/common/runtime.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("node_id", index_strategy, node_id_indexing,
            "assigns tasks to nodes based on list of nodes ids in file");


void
node_id_indexing::init_factory_params(sprockit::sim_parameters* params)
{
  node_id_indexing::init_factory_params(params);
  listfile_ = params->get_param("launch_node_id_file");
}

void
node_id_indexing::allocate(
  const app_id& aid,
  const node_set &nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  std::vector<hw::coordinates> node_list;
  node_id_allocation::read_coordinate_file(listfile_, result, topology_);
}

}
}
