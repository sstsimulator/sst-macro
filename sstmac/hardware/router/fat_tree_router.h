/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
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
  FactoryRegister("fattree | ftree", router, fat_tree_router)
 public:
  virtual ~fat_tree_router();

  fat_tree_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw);

  std::string to_string() const override {
    return "fat tree router";
  }

 private:
  /**
   * @brief build_rng
   * Build the random number generator for selecting paths
   */
  void build_rng();

  void route_to_switch(
    switch_id sw_addr,
    routable::path& path) override;

  /**
   * @brief choose_up_path
   * @return The selected path from the redundant (equivalent) set of minimal paths
   */
  int choose_up_minimal_path();

  /**
   * @brief number_paths
   * @param pkt The packet being routed by the fat-tree
   * @return The number of equivalent paths the packet can traverse
   *    on a minimal path to its destination switch.
   */
  int number_minimal_paths(packet* pkt) const;

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