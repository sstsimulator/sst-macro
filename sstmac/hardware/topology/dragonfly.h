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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_DRAGONFLY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_DRAGONFLY_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

class dragonfly : public cartesian_topology
{
  FactoryRegister("dragonfly | dfly", topology, dragonfly)
 public:
  dragonfly(sprockit::sim_parameters* params);

  typedef enum {
    x_dimension = 0,
    y_dimension = 1,
    g_dimension = 2
  } dimension_t;

  typedef enum {
    upX_vc = 0,
    downX_vc = 1
  } x_vc_t;

  typedef enum {
    upY_vc = 0,
    downY_vc = 1
  } y_vc_t;

 public:
  std::string to_string() const override {
    return "dragonfly";
  }

  dimension_t dim_for_port(int port){
    if (port >= (x_ + y_)){
      return g_dimension;
    } else if (port >= x_){
      return y_dimension;
    } else {
      return x_dimension;
    }
  }

  bool uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  bool uniform_switches() const override {
    return true;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
        sprockit::sim_parameters *switch_params) const override;

  virtual ~dragonfly() {}

  int ndimensions() const {
    return 3;
  }

  int numX() const {
    return x_;
  }

  int numY() const {
    return y_;
  }

  int numG() const {
    return g_;
  }

  int group_con() const {
    return group_con_;
  }

  inline void get_coords(switch_id sid, int& x, int& y, int& g) const {
    x = computeX(sid);
    y = computeY(sid);
    g = computeG(sid);
  }

  int get_uid(int x, int y, int g) const {
    return g * (x_ * y_) + y * (x_) + x;
  }

  inline int x_port(int x) const {
    return x;
  }

  inline int y_port(int y) const {
    return x_ + y;
  }

  inline int g_port(int g) const {
    return x_ + y_ + g;
  }

  inline int computeX(switch_id sid) const {
    return sid % x_;
  }

  inline int computeY(switch_id sid) const {
    return (sid / x_) % y_;
  }

  inline int computeG(switch_id sid) const {
    return (sid / (x_*y_));
  }

  int num_switches() const override {
    return x_ * y_ * g_;
  }

  int num_leaf_switches() const override {
    return x_ * y_ * g_;
  }

  void minimal_route_to_switch(
      switch_id current_sw_addr,
      switch_id dest_sw_addr,
      routable::path &path) const override;

  int minimal_distance(switch_id src, switch_id dst) const override;

  virtual int diameter() const override {
    return 5;
  }

  virtual switch_id random_intermediate_switch(switch_id current_sw,
                             switch_id dest_sw = switch_id(-1)) override;

  void configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  virtual void new_routing_stage(routable* rtbl) override;

  virtual void configure_geometric_paths(std::vector<int> &redundancies);

  coordinates switch_coords(switch_id) const override;

  switch_id switch_addr(const coordinates &coords) const override;

 protected:
  virtual void find_path_to_group(int myX, int myY, int myG, int dstG,
                     int& dstX, int& dstY,
                     routable::path& path) const;

  bool find_y_path_to_group(int myX, int myG, int dstG, int& dstY,
                       routable::path& path) const;

  bool find_x_path_to_group(int myY, int myG, int dstG, int& dstX,
                       routable::path& path) const;

  virtual bool xy_connected_to_group(int myX, int myY, int myG, int dstG) const;

 protected:
  int x_;
  int y_;
  int g_;
  int group_con_;
  bool true_random_intermediate_;

  void setup_port_params(sprockit::sim_parameters* params,
                    int dim, int dimsize) const;

  static std::string set_string(int x, int y, int g)
  {
    return sprockit::printf("{ %d %d %d }", x, y, g);
  }

 private:
  int convert_to_port(int dim, int dir) const {
    if      (dim == x_dimension) return x_port(dir);
    else if (dim == y_dimension) return y_port(dir);
    else if (dim == g_dimension) return g_port(dir);
    else return -1;
  }

  int xyg_dir_to_group(int myX, int myY, int myG, int dir) const;

};

}
} //end of namespace sstmac

#endif