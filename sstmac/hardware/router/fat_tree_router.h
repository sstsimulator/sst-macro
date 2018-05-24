/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sstmac/hardware/router/minimal_routing.h>
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
  FactoryRegister("fat_tree", router, fat_tree_router)
 public:
  fat_tree_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw);

  virtual ~fat_tree_router() {}

  std::string to_string() const override {
    return "fat tree router";
  }

  void route(packet* pkt) override;

  void rotate_up_next();

  void rotate_subtree_next(int tree);

  void rotate_leaf_next(int leaf);

  //int get_up_port(int next_tree);
  int get_up_port();

  int get_core_down_port(int next_tree);

  int get_agg_down_port(int dst_leaf);

  int num_vc() const override { return 1; }

 private:

  fat_tree* ft_;

  // routing -- up (is easy)
  std::vector<int> up_fwd_;
  int up_next_;
  // routing -- down (a little more complicated)
  std::map<int,std::vector<int>> subtree_fwd_;
  std::map<int,int> subtree_next_;
  std::map<int,std::vector<int>> leaf_fwd_;
  std::map<int,int> leaf_next_;
};

}
}
#endif
