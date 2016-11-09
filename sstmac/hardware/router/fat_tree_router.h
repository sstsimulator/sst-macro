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

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/common/rng.h>

namespace sstmac {
namespace hw {

/**
 * @brief The fat_tree_router class
 * Router encapsulating the special routing computations that must occur on
 * a fat tree topology.
 */
class fat_tree_router :
  public router
{
 public:
  virtual ~fat_tree_router();

  fat_tree_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw);

  //void
  //productive_paths_to_switch(switch_id dst, structured_routable::path_set &paths);

  virtual std::string
  to_string() const override {
    return "fat tree router";
  }

 private:
  /**
   * @brief build_rng
   * Build the random number generator for selecting paths
   */
  void
  build_rng();

  void
  route_to_switch(
    switch_id sw_addr,
    routable::path& path) override;

  /**
   * @brief choose_up_path
   * @return The selected path from the redundant (equivalent) set of minimal paths
   */
  int
  choose_up_minimal_path();

  /**
   * @brief number_paths
   * @param pkt The packet being routed by the fat-tree
   * @return The number of equivalent paths the packet can traverse
   *    on a minimal path to its destination switch.
   */
  int
  number_minimal_paths(packet* pkt) const;


 private:
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

  fat_tree* ftree_;


  int numpicked_;
  int pickstart_;

  int numpicktop_;
  int pickstarttop_;



};



}
}
#endif

