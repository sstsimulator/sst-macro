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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HYPERCUBE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HYPERCUBE_H_INCLUDED

#include <sstmac/hardware/topology/hdtorus.h>

namespace sstmac {
namespace hw {

class hypercube :
  public hdtorus
{
  FactoryRegister("hypercube", topology, hypercube,
              "hypercube implements a high-dimension torus with an arbitrary number of dimensions")
 public:
  hypercube(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "hdtorus topology";
  }

  virtual ~hypercube() {}

  void minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    routable::path& path) const override;

  bool uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
           sprockit::sim_parameters *switch_params) const override;

  inline int convert_to_port(int dim, int dir) const {
    return dim_to_outport_[dim] + dir;
  }

  int minimal_distance(switch_id src, switch_id dst) const override;

 protected:
  int radix_;
  int ndim_;
  std::vector<int> dim_to_outport_;

};

}
} //end of namespace sstmac

#endif // HYPERCUBE_H