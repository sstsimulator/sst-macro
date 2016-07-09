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

#ifndef SSTMAC_SOFTWARE_SERVICES_LAUNCH_INDEXING_FROMFILEINDEXER_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_LAUNCH_INDEXING_FROMFILEINDEXER_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/launch/task_mapper.h>

namespace sstmac {
namespace sw {


class dumpi_task_mapper : public task_mapper
{

 public:
  void
  set_topology(hw::topology *top);

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual ~dumpi_task_mapper() throw() {}

  void map_ranks(const app_id& aid,
                const ordered_node_set& nodes,
                int ppn,
                std::vector<node_id> &result,
                int nproc);
 protected:
  node_id
  node_id_from_hostname(const std::string& hostname);

  node_id
  node_id_from_coordinates(int ncoord, int* coordinates);

 protected:
  std::string metaname_;

  hw::structured_topology* regtop_;

};

}
}
#endif

