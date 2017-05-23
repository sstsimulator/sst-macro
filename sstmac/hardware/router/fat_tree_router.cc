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

#include <sstmac/hardware/router/fat_tree_router.h>
#include <sstmac/hardware/router/routable.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <cmath>

#define ftree_rter_debug(...) \
  rter_debug("fat tree: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

fat_tree_router::fat_tree_router(sprockit::sim_parameters* params, topology *top,
                                 network_switch *netsw) :
  router(params, top, netsw, routing::minimal),
  rng_(nullptr)
{
  ftree_ = safe_cast(fat_tree, top);
  k_ = ftree_->k();
  l_ = ftree_->l();
  seed_ = params->get_optional_long_param("router_seed", -1);
  build_rng();

  int switchesperlevel = pow(k_, l_ - 1);
  myL_ = my_addr_ / switchesperlevel;

  num_leaf_switches_reachable_ = pow(k_, myL_);
  num_leaf_switches_per_path_ = num_leaf_switches_reachable_ / k_;
  int level_relative_id = my_addr_ - myL_ * switchesperlevel;

  int my_leaf_group = level_relative_id / num_leaf_switches_reachable_;
  min_reachable_leaf_id_ = my_leaf_group * num_leaf_switches_reachable_;
  max_reachable_leaf_id_ = min_reachable_leaf_id_ + num_leaf_switches_reachable_;

  numpicked_ = 0;
  numpicktop_ = 0;
}

void
fat_tree_router::build_rng()
{
  long seed = 0;
  if (seed_ == -1){
    seed = time(NULL);
  } else {
    seed = seed_;
  }

  std::vector<RNG::rngint_t> seeds;
  seeds.push_back(seed);
  rng_ = RNG::Combo::construct(seeds);
}

fat_tree_router::~fat_tree_router()
{
  if (rng_) delete rng_;
}

#if 0
void
fat_tree_router::productive_paths_to_switch(
  switch_id dst, structured_routable::path_set &paths)
{
  structured_routable::path tmp_path;
  minimal_route_to_switch(dst, tmp_path);
  int dim = tmp_path.outport / k_;
  int dir = tmp_path.outport % k_;
  if (dim == fat_tree::down_dimension) {
    //we have no choice - only one path down is correct
    paths.resize(1);
    paths[0] = tmp_path;
  }
  else {
    // we have k productive paths up
    paths.resize(k_);
    for (int i=0; i < k_; ++i) {
      //paths[i].dim = fat_tree::up_dimension;
      //paths[i].dir = i;
      paths[i].vc = 0;
      paths[i].outport = ftree_->up_port(i);
    }
  }
}
#endif

void
fat_tree_router::route_to_switch(
  switch_id ej_addr,
  routable::path& path)
{
  int pathDir;
  int ej_id = ej_addr;
  int myAddr = my_addr_;
  if (ej_id >= min_reachable_leaf_id_ && ej_id < max_reachable_leaf_id_) {
    path.vc = 1;
    long relative_ej_id = ej_id - min_reachable_leaf_id_;
    pathDir = relative_ej_id / num_leaf_switches_per_path_;
    ftree_rter_debug("routing down with dir %d: eject-id=%ld rel-eject-id=%ld",
        pathDir, ej_id, relative_ej_id);
    path.outport = ftree_->down_port(pathDir);
  }
  else {
    //route up
    pathDir = choose_up_minimal_path();
    path.outport = ftree_->up_port(pathDir);
    path.vc = 0;
    ftree_rter_debug("routing up with dir %d", pathDir);
  }
}

int
fat_tree_router::choose_up_minimal_path()
{
  int ret = numpicked_;
  numpicked_ = (numpicked_ + 1) % k_;
  return ret;
}

int
fat_tree_router::number_minimal_paths(packet* pkt) const
{
  switch_id ej_addr = top_->netlink_to_ejection_switch(pkt->toaddr());
  long ej_id = ej_addr;
  if (ej_addr == my_addr_) {
    return 1;
  }
  else if (ej_id >= min_reachable_leaf_id_ && ej_id < max_reachable_leaf_id_) {
    return 1;
  }
  else {
    //route up
    return k_;
  }
}


}
}