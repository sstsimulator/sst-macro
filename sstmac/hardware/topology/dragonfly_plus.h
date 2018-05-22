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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_dragonfly_plus_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_dragonfly_plus_H_INCLUDED

#include <sstmac/hardware/topology/dragonfly.h>

namespace sstmac {
namespace hw {

/**
 * @brief The dragonfly class
 * A canonical dragonfly with notation/structure matching the Dally paper
 * Technology-Driven, Highly-Scalable Dragonfly Topology
 */
class dragonfly_plus : public dragonfly
{
  FactoryRegister("dragonfly_plus", topology, dragonfly_plus)

 public:
  dragonfly_plus(sprockit::sim_parameters* params);

 public:
  std::string to_string() const override {
    return "dragonfly+";
  }

  bool uniform_network_ports() const override {
    return false;
  }

  bool is_global_port(int port) const {
    return port >= 2*a_;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches() const override {
    return false;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
        sprockit::sim_parameters *switch_params) const override;

  virtual ~dragonfly_plus() {}

  int ndimensions() const {
    return 3;
  }

  /**
   * @brief get_coords
   * @param sid
   * @param a
   * @param g
   */
  inline void get_coords(switch_id sid, int& row, int& a, int& g) const {
    row = sid / num_leaf_switches_;
    a = sid % a_;
    g = (sid % num_leaf_switches_) / a_;
  }

  int get_uid(int row, int a, int g) const {
    return row*num_leaf_switches_ + g*a_ + a;
  }

  inline int computeRow(switch_id sid) const {
    return sid / num_leaf_switches_;
  }

  inline int computeA(switch_id sid) const {
    return sid % a_;
  }

  inline int computeG(switch_id sid) const {
    return (sid % num_leaf_switches_) / a_;
  }

  int num_switches() const override {
    return 2 * a_ * g_;
  }

  int num_leaf_switches() const override {
    return num_leaf_switches_;
  }

  void minimal_route_to_switch(
      int& path_rotater,
      switch_id current_sw_addr,
      switch_id dest_sw_addr,
      packet::path &path) const;

  int minimal_distance(switch_id src, switch_id dst) const override;

  int diameter() const override {
    return 5;
  }

  coordinates switch_coords(switch_id sid) const override {
    coordinates c(2);
    c[0] = computeRow(sid);
    c[1] = computeA(sid);
    c[2] = computeG(sid);
    return c;
  }

  switch_id switch_addr(const coordinates &coords) const override {
    return get_uid(coords[0], coords[1], coords[2]);
  }

 private:
  int num_leaf_switches_;
};

}
} //end of namespace sstmac

#endif
