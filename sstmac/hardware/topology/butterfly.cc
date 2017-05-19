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

#include <sstmac/hardware/topology/butterfly.h>
#include <sprockit/sim_parameters.h>

#include <math.h>

namespace sstmac {
namespace hw {

void
butterfly::minimal_route_to_switch(switch_id src,
                                    switch_id dst,
                                    routable::path &path) const
{
  int col = src / nswitches_per_col_;
  long group_size = nswitches_per_col_;
  for (int l=0; l <= col; ++l){
    group_size /= kary_;
  }
  long group_relative_row = dst % group_size;
  path.outport = group_relative_row / group_size / kary_;
  path.vc = 0;
}

abstract_butterfly::abstract_butterfly(sprockit::sim_parameters* params,
                                       InitMaxPortsIntra i1,
                                       InitGeomEjectID i2)
  : structured_topology(override_params(params), i1, i2)
{
}

sprockit::sim_parameters*
abstract_butterfly::override_params(sprockit::sim_parameters* params)
{
  //a 4-ary 3-fly has three levels of router
  //assume for simplicity nps = kary
  //we have 4^3 = 64 nodes
  //we need 4^(3-1) = 16 switches per level to support 64 nodes

  std::vector<int> args;
  params->get_vector_param("geometry", args);
  kary_ = args[0];
  nfly_ = args[1];
  if (!params->has_param("concentration")){
    params->add_param_override("concentration", kary_);
  }
  nswitches_per_col_ = pow(kary_, nfly_ - 1);
  return params;
}

void
abstract_butterfly::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1;
  m[routing::minimal_adaptive] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

butterfly::butterfly(sprockit::sim_parameters* params) :
  abstract_butterfly(params,
                     InitMaxPortsIntra::I_Remembered,
                     InitGeomEjectID::I_Remembered)
{
  last_col_index_start_ = nswitches_per_col_ * (nfly_ - 1);
  max_ports_intra_network_ = kary_;
  eject_geometric_id_ = max_ports_intra_network_;
}

void
butterfly::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  /**
    In 4-ary 3-fly, we have 16 switches per col
    with 3 columns or stages.  Thus we can label
    each switch by its row,col index
    Lets label switch 0 in each col A0, B0, C0...
    B0 = 16, B1 = 17 when using absolute indexing
    C0 = 32, C1 = 33 when using absolute indexing
    The first two cols connect in strides of 4
    A0 -> B0, B4, B8, B12 = 16, 20, 24, 28
    A1 -> B1, B5, B9, B13 = 17, 21, 25, 29
    ...
    A4 -> B0, B4, B8, B12
    etc
    The second and third cols connect in groups of 4
    with stride 1
    B0 -> C0, C1, C2, C3 = 32, 33, 34, 35
    B1 -> C0, C1, C2, C3
    B2 -> C0, C1, C2, C3
    B3 -> C0, C1, C2, C3
    B4 -> C4, C5, C6, C7 = 36, 37, 38, 39
    etc

    We distinguish between blocks and groups.
    A block is set of switches whose links "cross" and
    cannot be separated.  A group is a set of switches
    who connect to the same partners.
    On dimension 0, connecting A to B,
    we have one block of size 16 and 4 groups of size 4.
    On dimension1, connecting B to C,
    we now have four blocks of size 4 and still
    4 groups of size 4.
  */

  int cidx = 0;
  long connection_stride = nswitches_per_col_ / kary_;
  long block_size = nswitches_per_col_;
  conns.resize(kary_);
  for (int l=0; l < (nfly_-1); ++l) {
    int col_start_index = l * nswitches_per_col_;
    int col_stop_index = col_start_index + nswitches_per_col_;
    if (src >= col_start_index && src < col_stop_index){
      int i = src - col_start_index; //my offset in col

      int my_block = i / block_size;
      int my_intra_block_index = i % block_size;
      int my_block_index_start = my_block * block_size;

      int my_group_offset = my_intra_block_index % connection_stride;
      int my_group_index_start = my_block_index_start + my_group_offset;
      int my_index_in_my_group = (src - my_group_index_start) / connection_stride;


      int dst = my_group_index_start + col_start_index +
                              nswitches_per_col_;

      int inport = my_index_in_my_group;
      for (int c=0; c < kary_; ++c, dst += connection_stride) {
        connection& conn = conns[cidx];
        conn.src = src;
        conn.dst = dst;
        conn.src_outport = c;
        conn.dst_inport = inport;
        ++cidx;
      }
      break; //we are done
    }
    connection_stride /= kary_;
    block_size /= kary_;
  }
}

void
butterfly::configure_individual_port_params(switch_id src,
                      sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, max_ports_intra_network_,
                                             switch_params);
}

int
butterfly::minimal_distance(switch_id src, switch_id dst) const
{
  spkt_throw(sprockit::unimplemented_error, "butterfly::minimal_distance");
}

switch_id
butterfly::netlink_to_ejection_switch(node_id addr, int& switch_port) const
{
  long node_idx = addr;
  //we inject on the first row - eject on the last row
  long ej_idx = node_idx / netlinks_per_switch_ + last_col_index_start_;
  switch_port = node_idx % netlinks_per_switch_;
  return switch_id(ej_idx);
}


void
butterfly::nodes_connected_to_ejection_switch(switch_id swaddr,
                                              std::vector<injection_port>& nodes) const
{
  int last_row_offset = nswitches_per_col_ * (nfly_ - 1);
  if (swaddr >= last_row_offset) {
    switch_id sid_offset(swaddr - last_row_offset);
    return structured_topology::nodes_connected_to_injection_switch(sid_offset, nodes);
  } else {
    nodes.resize(0);
  }
}

void
butterfly::nodes_connected_to_injection_switch(switch_id swaddr,
                                               std::vector<injection_port>& nodes) const
{
  if (swaddr >= nswitches_per_col_) {
    nodes.resize(0);
  }
  else {
    return structured_topology::nodes_connected_to_injection_switch(swaddr, nodes);
  }
}

}
} //end of namespace sstmac