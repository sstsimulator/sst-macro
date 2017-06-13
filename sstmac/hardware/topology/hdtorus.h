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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HDTORUS_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HDTORUS_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

/**
 * @class hdtorus
 * Implements a high dimensional torus network.
 */

class hdtorus :
  public cartesian_topology
{
  FactoryRegister("torus | hdtorus", topology, hdtorus,
              "hdtorus implements a high-dimension torus with an arbitrary number of dimensions")
 public:
  hdtorus(sprockit::sim_parameters* params);

  typedef enum {
    pos = 0,
    neg = 1
  } direction_t;

  virtual std::string to_string() const override {
    return "hdtorus";
  }

  virtual ~hdtorus() {}

  int diameter() const override {
    return diameter_;
  }

  /// Returns the vector giving each dimension of the torus.
  const std::vector<int>& dimensions() const {
    return dimensions_;
  }

  coordinates neighbor_at_port(switch_id sid, int port);

  int num_switches() const override {
    return num_switches_;
  }

  int num_leaf_switches() const override {
    return num_switches();
  }

  bool uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches() const override {
    return true;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
            sprockit::sim_parameters *switch_params) const override;

  void minimal_route_to_switch(
    switch_id sid,
    switch_id dst,
    routable::path& path) const override;

  int minimal_distance(
    switch_id sid,
    switch_id dst) const override;

  void configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  coordinates switch_coords(switch_id) const override;

  switch_id switch_addr(const coordinates &coords) const override;


 protected:
  inline int convert_to_port(int dim, int dir) const {
    return 2*dim + dir;
  }

 private:
  void torus_path(bool reset_dim, bool wrapped, int dim, int dir,
             routable::path& path) const;

  void down_path(
    int dim, int src, int dst,
    routable::path& path) const;

  void up_path(
    int dim, int src, int dst,
    routable::path& path) const;

  int shortest_distance(int dim, int src, int dst) const;

  bool shortest_path_positive(
    int dim, int src, int dst) const;

 protected: //must be visible to hypercube
  int diameter_;
  long num_switches_;

};

}
} //end of namespace sstmac

#endif