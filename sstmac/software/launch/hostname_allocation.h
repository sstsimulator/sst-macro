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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_hostname_allocation_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_hostname_allocation_H_INCLUDED

#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/dumpi_util/dumpi_meta.h>

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {

namespace sw {
class hostname_allocation : public node_allocator
{

 public:
  hostname_allocation(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "hostname allocation";
  }

  /**
   * This function reads in a map file for mapping host names to network coordinates.
   * @param here the function making the call, used for more informative exceptions
   * @param mapfile name of the map file
   * @param hostmap map in which host mappings are placed
   */
  static void
  read_map_file(parallel_runtime* rt,
                const char* here,
                const std::string &mapfile,
                std::map<std::string,std::vector<int> >& hostmap);

  virtual void
  allocate(int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set &allocation) const override;

  virtual
  ~hostname_allocation() throw () {
  }

  typedef spkt_unordered_map<std::string, node_id> nodemap_t;
  static nodemap_t hostnamemap_;

  static std::map<long, std::string> nodenum_to_host_map_;

 protected:
  std::string mapfile_;

};

}
}

#endif /* hostname_allocation_H_ */

