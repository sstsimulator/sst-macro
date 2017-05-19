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

#include "xpressring.h"

namespace sstmac {
namespace hw {

xpress_ring::xpress_ring(sprockit::sim_parameters* params)
  : structured_topology(params, InitMaxPortsIntra::I_Remembered, InitGeomEjectID::I_Remembered)
{
  ring_size_ = params->get_int_param("xpress_ring_size");
  jump_size_ = params->get_int_param("xpress_jump_size");
  max_ports_intra_network_ = 4;
  eject_geometric_id_ = max_ports_intra_network_;
}

void
xpress_ring::connected_outports(switch_id src,
                            std::vector<connection>& conns) const
{
  conns.resize(4); //+1/-1 conns, +jump,-jump conns
  conns[0].src = src;
  conns[0].dst = (src+1) % ring_size_;
  conns[0].src_outport = up_port;
  conns[0].dst_inport = down_port;

  conns[1].src = src;
  conns[1].dst = (src+ring_size_ - 1) % ring_size_;
  conns[1].src_outport = down_port;
  conns[1].dst_inport = up_port;

  conns[2].src = src;
  conns[2].dst = (src+jump_size_) % ring_size_;
  conns[2].src_outport = jump_up_port;
  conns[2].dst_inport = jump_down_port;

  conns[3].src = src;
  conns[3].dst = (src-jump_size_+ring_size_) % ring_size_;
  conns[3].src_outport = jump_down_port;
  conns[3].dst_inport = jump_up_port;
}

void
xpress_ring::configure_individual_port_params(switch_id src, sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, 4, switch_params);
}

void
xpress_ring::configure_vc_routing(std::map<routing::algorithm_t, int>& m) const 
{
  m[routing::minimal] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

void
xpress_ring::minimal_route_to_switch(
  switch_id src,
  switch_id dest,
  routable::path& path) const
{
  //can route up or down
  int up_distance = abs(dest - src);
  int down_distance = abs(src + ring_size_ - dest);

  int xpress_cutoff = jump_size_ / 2;

  if (up_distance <= down_distance) {
    if (up_distance > xpress_cutoff) {
      path.outport = jump_up_port;
      path.vc = 0;
    }
    else {
      path.outport = up_port;
      path.vc = 0;
    }
  }
  else {
    if (down_distance > xpress_cutoff) {
      path.outport = jump_down_port;
      path.vc = 0;
    }
    else {
      path.outport = down_port;
      path.vc = 0;
    }
  }
}

int
xpress_ring::num_hops(int total_distance) const
{
  int num_jumps = total_distance / jump_size_;
  int num_steps = total_distance % jump_size_;
  int half_jump = jump_size_ / 2;
  if (num_steps > half_jump) {
    //take an extra jump
    ++num_jumps;
    num_steps = jump_size_ - num_steps;
  }
  return num_jumps + num_steps;
}

int
xpress_ring::minimal_distance(
  switch_id src,
  switch_id dest) const
{
  int up_distance = abs(dest - src);
  int down_distance = abs(src + ring_size_ - dest);

  int total_distance = std::max(up_distance, down_distance);
  return num_hops(total_distance);
}

int
xpress_ring::diameter() const
{
  //half-way around the ring is the biggest
  int halfway = ring_size_ / 2;
  return num_hops(halfway);
}



}
}