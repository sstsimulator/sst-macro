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

#ifndef SSTMAC_HARDWARE_NETWORK_SWTICHES_ROUTING_FATTREEROUTER_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWTICHES_ROUTING_FATTREEROUTER_H_INCLUDED

#include <sstmac/hardware/router/structured_router.h>
#include <sstmac/common/rng.h>

namespace sstmac {
namespace hw {

class fat_tree_router :
  public structured_router
{
 public:
  virtual
  ~fat_tree_router() {
  }

  virtual void
  finalize_init();

  virtual int
  choose_up_path();

  virtual int
  number_paths(message* msg) const;

  virtual void
  path_is_good(node_id goingto, int fromport, int toport) {
  }

  void
  productive_paths_to_switch(switch_id dst, geometry_routable::path_set &paths);

  virtual void
  path_teardown(int fromport, int toport) {
  }

  void
  init_factory_params(sprockit::sim_parameters *params);

  virtual std::string
  to_string() const {
    return "fattreerouter";
  }

  void
  route(packet* pkt);

 protected:
  bool
  is_top_level() const {
    return myL_ == l_ - 1;
  }

  void
  build_rng();

  void
  minimal_route_to_switch(
    switch_id sw_addr,
    geometry_routable::path& path);

 protected:
  int l_;
  int k_;

  int myL_;
  int logicalid_;

  std::map<long, int> inports_;
  RNG::Combo* rng_;

  long num_leaf_switches_reachable_;
  long num_leaf_switches_per_path_;
  long level_relative_id_;
  long min_reachable_leaf_id_;
  long max_reachable_leaf_id_;
  long seed_;


  int numpicked_;
  int pickstart_;

  int numpicktop_;
  int pickstarttop_;



};



}
}
#endif

