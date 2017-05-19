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

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

class xpress_ring :
  public structured_topology
{
  FactoryRegister("xpress", topology, xpress_ring,
              "A ring topology with express cables that make large jumps")
 public:
  typedef enum {
    up_port = 0,
    down_port = 1,
    jump_up_port = 2,
    jump_down_port = 3
  } port_t;

 public:
  xpress_ring(sprockit::sim_parameters* params);

  virtual ~xpress_ring() {}

  bool uniform_switches() const override {
    return true;
  }

  bool uniform_network_ports() const override {
    return true;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  std::string to_string() const override {
    return "xpress ring topology";
  }

  void configure_individual_port_params(switch_id src,
              sprockit::sim_parameters* switch_params) const override;

  void configure_vc_routing(std::map<routing::algorithm_t, int>& m) const override;

  void connected_outports(switch_id src,
        std::vector<topology::connection>& conns) const override;

  /**
  Workhorse function for implementing #minimal_route_to_switch
  and #minimal_route_to_node.
  Given source/dest coordinates, find the minimal path.
  @param current_sw_addr The addr of the current switch
  @param dest_sw_addr The addr of the destination switch
  @param path [inout] A complete path descriptor to the destination switch
  */
  void minimal_route_to_switch(
    switch_id src,
    switch_id dest,
    routable::path& path) const override;

  /**
  The function accepts either source or node coordinates.
  This gives the minimal distance counting the number of hops between switches.
  If node coordinates are given, the last coordinate is just ignored.
  @param src_coords. The source coordinates. This can be either switch or node coordinates.
  @param dest_coords. The destination coordinates. This can be either switch or node coordinates.
  @return The number of hops to final destination
  */
  int minimal_distance(
    switch_id src,
    switch_id dest) const override;

  int num_switches() const override {
    return ring_size_;
  }

  int diameter() const override;

 private:
  int num_hops(int total_distance) const;

 private:
  int ring_size_;

  int jump_size_;

};

}
}