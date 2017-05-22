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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_butterfly_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_butterfly_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 * @brief The abstract_butterfly class
 * Encapsulates operations common to both butterfly and flattened_butterfly
 */
class abstract_butterfly :
  public structured_topology
{
 public:
  typedef enum {
    up_dimension = 0,
    down_dimension = 1
  } dimension_t;

 public:
  virtual ~abstract_butterfly() {}

  /**
   * @brief kary
   * @return The branching degree of the butterfly
   */
  int kary() const {
    return kary_;
  }

  /**
   * @brief nfly
   * @return The number of stages in the butterfly
   */
  int nfly() const {
    return nfly_;
  }

  /**
   * @brief num_switches_per_col
   * The butterfly is physically laid out as a 2D-graid of cols and rows
   * @return The number of switches in a column of the 2D physical layout
   */
  int num_switches_per_col() const {
    return nswitches_per_col_;
  }

  int diameter() const {
    return nfly_ + 1;
  }

  void configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

 protected:
  abstract_butterfly(sprockit::sim_parameters* params,
                     InitMaxPortsIntra i1,
                     InitGeomEjectID i2);

 protected:
  int kary_;
  int nfly_;
  long nswitches_per_col_;

 private:
  sprockit::sim_parameters* override_params(sprockit::sim_parameters* params);

};

/**
 * @brief The butterfly class
 * Encapsulates a butterfly topology as described in "High Performance Datacenter Networks"
 * by Abts and Kim
 */
class butterfly :
  public abstract_butterfly
{
  FactoryRegister("butterfly | bfly", topology, butterfly)
 public:
  butterfly(sprockit::sim_parameters* params);

  virtual std::string to_string() const override {
    return "butterfly";
  }

  virtual ~butterfly() {}

  int num_switches() const override {
    return nswitches_per_col_ * nfly_;
  }

  int num_leaf_switches() const override {
    return nswitches_per_col_;
  }

  int minimal_distance(switch_id src, switch_id dst) const override;

  void minimal_route_to_switch(switch_id current_sw_addr,
                          switch_id dest_sw_addr,
                          routable::path &path) const override;

  bool uniform_switches() const override {
    return true;
  }

  bool uniform_network_ports() const override {
    return true;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
                        sprockit::sim_parameters *switch_params) const override;

  switch_id netlink_to_ejection_switch(
    node_id nodeaddr,
    int &switch_port) const override;

  void nodes_connected_to_injection_switch(switch_id swaddr,
                                      std::vector<injection_port>& nodes) const override;

  void nodes_connected_to_ejection_switch(switch_id swaddr,
                                     std::vector<injection_port>& nodes) const override;

 private:
  int last_col_index_start() const {
    return last_col_index_start_;
  }

 private:
  long last_col_index_start_;

};

}
} //end of namespace sstmac

#endif