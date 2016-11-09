

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

#include <sstmac/software/launch/node_id_task_mapper.h>
#include <sstmac/software/launch/node_id_allocation.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/common/runtime.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"launch_node_id_file",
"launch_node_id_mapper_file",
"launch_node_id_indexing_file",
"node_id_indexing_file",
"node_id_file",
"node_id_mapper_file",
);

namespace sstmac {
namespace sw {

SpktRegister("node_id", task_mapper, node_id_task_mapper,
            "assigns tasks to nodes based on list of nodes ids in file");

node_id_task_mapper::node_id_task_mapper(sprockit::sim_parameters* params) :
  task_mapper(params)
{
  if (params->has_param("node_id_file")){
    listfile_ = params->get_param("node_id_file" );
  } else {
    listfile_ = params->get_param("node_id_indexing_file");
  }
}

void
node_id_task_mapper::map_ranks(
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  node_id_allocation::read_coordinate_file(listfile_, result, topology_);
  if (result.size() != nproc){
    spkt_throw_printf(sprockit::unimplemented_error,
      "need %d nodes - only indexed %d", nproc, nodes.size());
  }
}

}
}
