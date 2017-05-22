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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FAKE_TOPOLOGY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FAKE_TOPOLOGY_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

class fake_topology :
  public structured_topology
{
 public:
  virtual std::string to_string() const override {
    return "fake topology";
  }

  virtual ~fake_topology() {}

  switch_id switch_number(const coordinates& coords) const {
    return switch_id();
  }

  virtual void productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    routable::path& path) const {
  }

  int num_hops(node_id src, node_id dst) const {
    return 1;
  }

  void connect_objects(internal_connectable_map& objects) {
    //do nothing
  }

  void minimal_route_to_coords(
    const coordinates &current_coords,
    const coordinates &dest_coords,
    routable::path& path) const {
    //do nothing
  }

  int minimal_distance(
    const coordinates &current_coords,
    const coordinates &dest_coords) const {
    return 0;
  }

  virtual int diameter() const {
    spkt_throw(sprockit::unimplemented_error,
              "switchinterconnect::fake_topology::diameter: don't call this");
  }

  switch_id switch_number(const std::vector<int>& coords) const {
    return switch_id();
  }

  node_id node_addr(const std::vector<int>& coords) const {
    return node_id();
  }

  int num_switches() const {
    return 0;
  }

  int convert_to_port(int dim, int dir) const {
    return 0;
  }

  void convert_to_dimdir(int outport, int &dim, int&dir) const {
    dim = 0;
    dir = 0;
  }


 protected:
  void compute_switch_coords(switch_id swid, coordinates &coords) const {
    //do nothing
  }


};
}
}

#endif