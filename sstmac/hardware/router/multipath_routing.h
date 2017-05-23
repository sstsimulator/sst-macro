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

#ifndef MULTIPATH_ROUTER_H
#define MULTIPATH_ROUTER_H

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/hardware/topology/multipath_topology.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

template <class ParentRouter>
class multipath_router :
  public ParentRouter
{
  /**
   * @brief The structured_path struct Identifies a (structurally) unique
   * path in the topology. For example, there might be multiple links on a
   * router than connect to the +X router in a torus. However,
   * these links are all considered to be ``structurally'' equivalent.
   */
  struct multipath {
    /**
     * @brief redundancy How many redundant physical links compose the single structural link
     */
    int redundancy;

    /**
     * @brief path_counter The index of the last redundant path taken
     */
    int path_counter;

    multipath() : path_counter(0) {}

    /**
     * @brief next_index
     * @return The next redundant path that should be taken based on the previously taken paths
     */
    int next_index() {
      int ret = path_counter;
      path_counter = (path_counter + 1) % redundancy;
      return ret;
    }
  };

  virtual void compatibility_check(){
    //do nothing
  }

 public:
  multipath_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw) :
    ParentRouter(params, top, netsw),
    top_(safe_cast(multipath_topology, top))
  {
    std::vector<int> reds;
    top_ = safe_cast(multipath_topology, top);
    top_->configure_geometric_paths(reds);
    int npaths = reds.size();
    geom_paths_.resize(npaths);
    for (int i=0; i < npaths; ++i){
      geom_paths_[i].redundancy = reds[i];
    }
  }

  virtual void route(packet* pkt){
    routable::path_set paths;
    ParentRouter::route(pkt);
    routable::path& path = pkt->interface<routable>()->current_path();
    top_->get_redundant_paths(path, paths);

    int path_id = paths[0].geometric_id;
    int next_index = geom_paths_[path_id].next_index();
    debug_printf(sprockit::dbg::router,
      "multipath routing: using index %d", next_index);
    path = paths[next_index];
  }

 private:
  std::vector<multipath> geom_paths_;
  multipath_topology* top_;

};

}
}

#endif // MULTIPATH_ROUTER_H