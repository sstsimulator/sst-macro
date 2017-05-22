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

#ifndef TILED_TORUS_H
#define TILED_TORUS_H

#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/topology/multipath_topology.h>

namespace sstmac {
namespace hw {


class tiled_torus :
  public hdtorus,
  public multipath_topology
{
  FactoryRegister("tiled_torus | tiled_hdtorus", topology, tiled_torus)
 public:
  tiled_torus(sprockit::sim_parameters *params);

  void get_redundant_paths(routable::path& inPath,
                      routable::path_set& outPaths) const override;

  void configure_geometric_paths(std::vector<int>& redundancies) const override;

  switch_id netlink_to_injection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const override;

  bool uniform_network_ports() const override {
    return true;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  void connected_outports(switch_id src,
           std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
            sprockit::sim_parameters *switch_params) const override;

 private:
  inline int port(int replica, int dim, int dir) const {
    int offset = tile_offsets_[dim];
    //offset for dim, each replica has +/- for *2
    return offset + red_[dim] * dir + replica;
  }

 private:
  int ntiles_row_;
  int ntiles_col_;
  std::vector<int> tile_offsets_;
  std::vector<int> rotater_;
  int first_simple_torus_eject_port_;

};

}
}


#endif // TILED_TORUS_H