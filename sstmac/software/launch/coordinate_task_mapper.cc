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

#include <sstmac/software/launch/coordinate_task_mapper.h>
#include <sstmac/software/launch/coordinate_allocation.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/common/runtime.h>
#include <sprockit/fileio.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("coordinate", task_mapper, coordinate_task_mapper,
            "assigns tasks to nodes based on hostname map of topology and hostname list in file");


coordinate_task_mapper::coordinate_task_mapper(sprockit::sim_parameters *params) :
  task_mapper(params)
{
  listfile_ = params->get_param("coordinate_file");
}

void
coordinate_task_mapper::map_ranks(
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  hw::cartesian_topology* regtop = safe_cast(hw::cartesian_topology, topology_);

  std::vector<hw::coordinates> node_list;
  coordinate_allocation::read_coordinate_file(rt_, listfile_, node_list);
  int num_nodes = node_list.size();
  result.resize(num_nodes);
  for (int i=0; i < num_nodes; ++i){
    const hw::coordinates& coords = node_list[i];
    node_id nid = regtop->node_addr(coords);
    result[i] = nid;
  }
}

}
}


