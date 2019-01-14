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

#include "xpressring.h"

namespace sstmac {
namespace hw {

xpress_ring::xpress_ring(sprockit::sim_parameters* params)
  : StructuredTopology(params)
{
  ring_size_ = params->get_int_param("xpress_ring_size");
  jump_size_ = params->get_int_param("xpress_jump_size");
}

void
xpress_ring::connectedOutports(SwitchId src, std::vector<connection>& conns) const
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
xpress_ring::endpointsConnectedToInjectionSwitch(SwitchId swid,
                                                     std::vector<injection_port> &nodes) const
{
  nodes.resize(concentration());
  for (int i=0; i < concentration(); ++i){
    injection_port& port = nodes[i];
    port.ep_port = 0;
    port.switch_port = 4 + i;
    port.nid = concentration() * swid + i;
  }
}

void
xpress_ring::configureIndividualPortParams(SwitchId src, sprockit::sim_parameters *switch_params) const
{
  Topology::configureIndividualPortParams(0, 4, switch_params);
}

int
xpress_ring::numHopsToNode(NodeId src_node, NodeId dest_node) const
{
  SwitchId src = src_node / concentration_;
  SwitchId dest = dest_node / concentration_;

  int up_distance = abs(dest - src);
  int down_distance = abs(src + ring_size_ - dest);

  int total_distance = std::max(up_distance, down_distance);
  return num_hops_for_distance(total_distance);
}

int
xpress_ring::num_hops_for_distance(int total_distance) const
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
xpress_ring::diameter() const
{
  //half-way around the ring is the biggest
  int halfway = ring_size_ / 2;
  return num_hops_for_distance(halfway);
}



}
}
