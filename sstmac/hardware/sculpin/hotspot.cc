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

#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/hardware/sculpin/hotspot.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>

RegisterKeywords(
{ "hot_cutoff", "the queue depth at which counts should be 'hot'" },
{ "hot_color_fraction", "color normalization relative to regular activity" },
{ "vtk_face_depth_fraction", "the depth to draw a face volume when plotting with VTK" },
);

namespace sstmac {

stat_hotspot::stat_hotspot(sprockit::sim_parameters* params) :
  stat_collector(params),
  max_regular_activity_(std::numeric_limits<int>::min())
{
  hot_cutoff_ = params->get_int_param("hot_cutoff");
  hot_color_fraction_ = params->get_double_param("hot_color_fraction");
  vtk_face_depth_fraction_ = params->get_optional_double_param("vtk_face_depth_fraction", 0.25);
}

void
stat_hotspot::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;

  int num_switches = top_->num_switches();
  int root = 0;
  int* data = (int*) all_activities_.data();
  rt->global_sum(data, 2*6*num_switches, root);
}

void
stat_hotspot::reduce(stat_collector* contrib)
{
  stat_hotspot* other = safe_cast(stat_hotspot, contrib);

  int num_switches = top_->num_switches();
  if (all_activities_.empty()){
    all_activities_.resize(num_switches);
  }

  activity& this_activity = all_activities_[other->id()];
  for (int face=0; face < 6; ++face){
    this_activity.counts[0][face] += other->activity_.counts[0][face];
    this_activity.counts[1][face] += other->activity_.counts[1][face];

    int regular = other->activity_.counts[REGULAR_ACTIVITY][face];
    max_regular_activity_ = std::max(regular, max_regular_activity_);
  }
}

void
stat_hotspot::dump_global_data()
{
  std::string data_file = fileroot_ + ".xyz";
  std::fstream os;
  check_open(os, data_file);

  int num_switches = top_->num_switches();
  double regular_fraction = 1.0 / double(max_regular_activity_);
  double hot_fraction = hot_color_fraction_ * regular_fraction;
  for (int sid=0; sid < num_switches; ++sid){
    activity& act = all_activities_[sid];
    hw::topology::vtk_switch_geometry geom = top_->get_vtk_geometry(sid);
    hw::topology::output_box(os, geom.box, "gray", "0.15");
    os << "\n";
    //RGB tuples #0343DF
    for (int face=0; face < 6; ++face){
      int red_count = act.counts[HOT_ACTIVITY][face];
      int green_count = act.counts[REGULAR_ACTIVITY][face];

      if (red_count > 0 || green_count > 0){
        double red =  red_count * hot_fraction;
        double green = green_count * regular_fraction;
        hw::topology::vtk_box_geometry face_box = geom.box.get_face_geometry(
              hw::topology::vtk_face_t(face), vtk_face_depth_fraction_);

        hw::topology::output_box(os, face_box);
        os << ";rgb=" << red << "," << green << ",0"
           << ";alpha=0.5"
           << "\n";
      }
    }
  }

  os.close();
}

void
stat_hotspot::dump_local_data()
{
  return;
}

void
stat_hotspot::clear()
{
}

void
stat_hotspot::collect(int port, int queue_depth)
{
  //the behavior defined for this is that
  //if the port is not included in the mapping
  //ignore it
  if (port >= port_to_face_.size()) return;

  hw::topology::vtk_face_t face = port_to_face_[port];
  if (queue_depth >= hot_cutoff_){
    activity_.counts[HOT_ACTIVITY][face]++;
  } else {
    activity_.counts[REGULAR_ACTIVITY][face]++;
  }
}


}
