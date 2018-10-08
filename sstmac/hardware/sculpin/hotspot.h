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

#ifndef STAT_hotspot_sculpin_h
#define STAT_hotspot_sculpin_h

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/hardware/topology/topology.h>
#include <vector>

namespace sstmac {

class stat_hotspot :
  public stat_collector
{
  FactoryRegister("hotspot", stat_collector, stat_hotspot)
 public:
  stat_hotspot(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "stat hotspot";
  }

  void collect(int port, int queue_depth);

  void dump_local_data() override;

  void dump_global_data() override;

  void global_reduce(parallel_runtime *rt) override;

  void clear() override;

  void reduce(stat_collector* coll) override;

  void configure(int sid, hw::topology* top){
    sid_ = sid;
    top_ = top;
    auto geom = top_->get_vtk_geometry(sid);
    port_to_face_ = geom.port_faces;
  }

  stat_collector* do_clone(sprockit::sim_parameters* params) const override {
    auto clone = new stat_hotspot(params);
    clone->top_ = top_;
    return clone;
  }

 protected:
  enum {
    REGULAR_ACTIVITY = 0,
    HOT_ACTIVITY = 1
  };

  struct activity {
    int counts[2][6];
    activity(){
      ::memset(counts, 0, sizeof(counts));
    }
  };

  activity activity_;

  hw::topology* top_;

  std::vector<hw::topology::vtk_face_t> port_to_face_;

  int sid_;

  std::vector<activity> all_activities_;

  int max_regular_activity_;

  int hot_cutoff_;

  double hot_color_fraction_;

  double vtk_face_depth_fraction_;

};

}

#endif // STAT_hotspot_h
