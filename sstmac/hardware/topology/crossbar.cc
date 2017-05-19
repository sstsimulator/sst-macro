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

#include <sstream>
#include <sstmac/hardware/topology/crossbar.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

crossbar::crossbar(sprockit::sim_parameters* params) :
  structured_topology(params,
                      InitMaxPortsIntra::I_Remembered,
                      InitGeomEjectID::I_Remembered)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  size_ = args[0];
  max_ports_intra_network_ = num_switches();
  eject_geometric_id_ = max_ports_intra_network_;
}

void
crossbar::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1;
  m[routing::minimal_adaptive] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

void
crossbar::minimal_route_to_switch(switch_id current_sw_addr,
                                  switch_id dest_sw_addr,
                                  routable::path &path) const
{
  path.vc = 0;
  path.outport = dest_sw_addr;
}

void
crossbar::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int n_switches = num_switches();
  conns.resize(n_switches - 1);
  int cidx = 0;
  for (int i=0; i < n_switches; ++i){
    if (i == src) continue;

    conns[cidx].src = src;
    conns[cidx].dst = i;
    conns[cidx].src_outport = i;
    conns[cidx].dst_inport = src;
    ++cidx;
  }
}

void
crossbar::configure_individual_port_params(switch_id src, sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, num_switches(), switch_params);
}

}
} //end of namespace sstmac