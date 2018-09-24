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

#include <sstmac/software/launch/cart_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/common/cartgrid.h>

#include <sprockit/errors.h>
#include <sprockit/factories/factory.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "num_groups", "the number of groups to scatter an allocation across" },
);

#include <vector>
#include <sstmac/software/launch/node_allocator.h>

namespace sstmac {
namespace sw {

class greedy_dfly_allocation :
  public node_allocator
{
  FactoryRegister("greedy_dfly", node_allocator, greedy_dfly_allocation,
              "Allocate a 'striped' dragonfly allocation scattering across groups")
 public:
  greedy_dfly_allocation(sprockit::sim_parameters* params) : node_allocator(params) {
    num_groups_ = params->get_int_param("num_groups");
  }

  virtual ~greedy_dfly_allocation() throw () {}

  std::string to_string() const override {
    return "greedy dfly allocation";
  }

  /**
   * @brief allocate
   * @param nnode
   * @param available
   * @param allocation
   * @return  Whether the allocation succeeded based on the available nodes
   */
  bool allocate(int nnode,
    const ordered_node_set& available,
    ordered_node_set& allocation) const override {
    int num_per_group = nnode / num_groups_;
    if (nnode % num_groups_){
      num_per_group++;
    }

    hw::dragonfly* dfly = safe_cast(hw::dragonfly, topology_);
    int ng = dfly->g();
    int na = dfly->a();
    int conc = dfly->concentration();
    int num_remaining = nnode;
    for (int g=0; g < ng; ++g){
      int num_needed = std::min(num_remaining, num_per_group);
      int num_left = num_needed;
      std::set<int> nodes_this_group;
      for (int a=0; a < na; ++a){
        switch_id sid = dfly->get_uid(a,g);
        node_id nid_offset = sid*conc;
        for (int c=0; c < conc; ++c){
          node_id nid = nid_offset + c;
          //consider this node for addition to the allocation
          if (available.find(nid) != available.end()){
            num_left--;
            nodes_this_group.insert(nid);
            if (num_left == 0) break;
          }
        }
        if (num_left == 0) break;
      }
      if (num_left == 0){
       //this group has enough to satisfy our request
       //add the considered nodes to the allocation now
       num_remaining -= num_needed;
       for (auto nid : nodes_this_group) allocation.insert(nid);
      }
    }

    if (num_remaining > 0){
      //we failed to allocate the right number of nodes
      allocation.clear();
      return false;
    } else {
      return true;
    }
  }

 private:
  int num_groups_;

};

}
}

