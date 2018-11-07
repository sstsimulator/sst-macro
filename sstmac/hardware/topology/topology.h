/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_SWITCHTOPOLOGY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_SWITCHTOPOLOGY_H_INCLUDED

#include <sstmac/hardware/topology/coordinates.h>
#include <sstmac/hardware/topology/traffic/traffic.h>
#include <sstmac/hardware/router/router_fwd.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/common/packet.h>
#include <sstmac/backends/common/sim_partition_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/errors.h>
#include <unordered_map>
#include <cmath>

DeclareDebugSlot(topology)

#define top_debug(...) \
  debug_printf(sprockit::dbg::topology, __VA_ARGS__)

namespace sstmac {
namespace hw {

class topology : public sprockit::printable
{
  DeclareFactory(topology)

 public:
  struct connection {
    switch_id src;
    switch_id dst;
    int src_outport;
    int dst_inport;
  };

  typedef enum {
    plusXface = 0,
    plusYface = 1,
    plusZface = 2,
    minusXface = 3,
    minusYface = 4,
    minusZface = 5
  } vtk_face_t;

  struct injection_port {
    node_id nid;
    int switch_port;
    int ep_port;
  };

  struct rotation {
    double x[3];
    double y[3];
    double z[3];

    /**
     * @brief rotation Initialize as a 3D rotation
     * @param ux  The x component of the rotation axis
     * @param uy  The y component of the rotation axis
     * @param uz  The z component of the rotation axis
     * @param theta The angle of rotation
     */
    rotation(double ux, double uy, double uz, double theta){
      double cosTh = cos(theta);
      double oneMinCosth = 1.0 - cosTh;
      double sinTh = sin(theta);
      x[0] = cosTh + ux*ux*oneMinCosth;
      x[1] = ux*uy*oneMinCosth - uz*sinTh;
      x[2] = ux*uz*oneMinCosth + uy*sinTh;

      y[0] = uy*ux*oneMinCosth + uz*sinTh;
      y[1] = cosTh + uy*uy*oneMinCosth;
      y[2] = uy*uz*oneMinCosth - ux*sinTh;

      z[0] = uz*ux*oneMinCosth - uy*sinTh;
      z[1] = uz*uy*oneMinCosth + ux*sinTh;
      z[2] = cosTh + uz*uz*oneMinCosth;
    }

    /**
     * @brief rotation Initialize as a 2D rotation around Z-axis
     * @param theta The angle of rotation
     */
    rotation(double theta){
      double cosTh = cos(theta);
      double sinTh = sin(theta);
      x[0] = cosTh;
      x[1] = -sinTh;
      x[2] = 0.0;

      y[0] = sinTh;
      y[1] = cosTh;
      y[2] = 0.0;

      z[0] = 0.0;
      z[1] = 0.0;
      z[2] = 1.0;
    }

  };

  struct xyz {
    double x;
    double y;
    double z;

    xyz() : x(0), y(0), z(0) {}

    xyz(double X, double Y, double Z) :
      x(X), y(Y), z(Z){}

    double& operator[](int dim){
      switch(dim){
      case 0: return x;
      case 1: return y;
      case 2: return z;
      }
      return x;//never reached, keep compiler from warning
    }

    xyz operator+(const xyz& r) const {
      return xyz(x+r.x, y+r.y, z+r.z);
    }

    xyz rotate(const rotation& r) const {
      xyz ret;
      ret.x += r.x[0]*x + r.x[1]*y + r.x[2]*z;
      ret.y += r.y[0]*x + r.y[1]*y + r.y[2]*z;
      ret.z += r.z[0]*x + r.z[1]*y + r.z[2]*z;
      return ret;
    }
  };

  struct vtk_box_geometry {
    xyz size;
    xyz corner;
    rotation rot;

    xyz vertex(int id) const {
      switch(id){
      case 0:
        return corner.rotate(rot);
      case 1:
        return xyz(corner.x,corner.y+size.y,corner.z).rotate(rot);
      case 2:
        return xyz(corner.x,corner.y,corner.z+size.z).rotate(rot);
      case 3:
        return xyz(corner.x,corner.y+size.y,corner.z+size.z).rotate(rot);
      case 4:
        return xyz(corner.x+size.x,corner.y,corner.z).rotate(rot);
      case 5:
        return xyz(corner.x+size.x,corner.y+size.y,corner.z).rotate(rot);
      case 6:
        return xyz(corner.x+size.x,corner.y,corner.z+size.z).rotate(rot);
      case 7:
        return xyz(corner.x+size.x,corner.y+size.y,corner.z+size.z).rotate(rot);
      }
      spkt_abort_printf("vertex number should be 0-7: got %d", id);
      return xyz();
    }

    vtk_box_geometry(double xLength, double yLength, double zLength,
                 double xCorner, double yCorner, double zCorner,
                 double xAxis, double yAxis, double zAxis, double theta) :
      size(xLength,yLength,zLength),
      corner(xCorner, yCorner, zCorner),
      rot(xAxis, yAxis, zAxis, theta) {}

    vtk_box_geometry(double xLength, double yLength, double zLength,
                 double xCorner, double yCorner, double zCorner,
                 double theta) :
      size(xLength,yLength,zLength),
      corner(xCorner, yCorner, zCorner),
      rot(theta) {}

    vtk_box_geometry(double xLength, double yLength, double zLength,
                 double xCorner, double yCorner, double zCorner) :
      vtk_box_geometry(xLength, yLength, zLength, xCorner, yCorner, zCorner, 0.0)
   {}

    vtk_box_geometry(double xLength, double yLength, double zLength,
                     double xCorner, double yCorner, double zCorner,
                     const rotation& rot) :
      size(xLength,yLength,zLength),
      corner(xCorner,yCorner,zCorner),
      rot(rot)
    {
    }

    vtk_box_geometry get_sub_geometry(double x_start, double x_span,
                                      double y_start, double y_span,
                                      double z_start, double z_span) const {

      return vtk_box_geometry(size.x * x_span, size.y * y_span, size.z * z_span,
                              corner.x + size.x * x_start,
                              corner.y + size.y * y_start,
                              corner.z + size.z * z_start, rot);
    }

    xyz plus_x_corner() const {
      xyz loc = corner;
      loc.x += size.x;
      return loc;
    }

    xyz plus_y_corner() const {
      xyz loc = corner;
      loc.y += size.y;
      return loc;
    }

    xyz plus_z_corner() const {
      xyz loc = corner;
      loc.z += size.z;
      return loc;
    }

    xyz center() const {
      double newX = (corner.x + size.x*0.5);
      double newY = (corner.y + size.y*0.5);
      double newZ = (corner.z + size.z*0.5);
      return xyz(newX, newY, newZ).rotate(rot);
    }

    xyz x_anchor() const {
      //return the center of the front face
      double newX = corner.x;
      double newY = corner.y + size.y*0.5;
      double newZ = corner.z + size.y*0.5;
      return xyz(newX, newY, newZ).rotate(rot);
    }

  };

  struct vtk_switch_geometry {
    vtk_box_geometry box;
    struct port_geometry {
      double x_size;
      double y_size;
      double z_size;
      double x_offset;
      double y_offset;
      double z_offset;
    };
    std::vector<port_geometry> ports;

    vtk_box_geometry get_port_geometry(int port) const {
      auto& cfg = ports[port];
      return box.get_sub_geometry(cfg.x_offset, cfg.x_size,
                                  cfg.y_offset, cfg.y_size,
                                  cfg.z_offset, cfg.z_size);
    }

    vtk_switch_geometry(double xLength, double yLength, double zLength,
                 double xCorner, double yCorner, double zCorner,
                 double theta, std::vector<port_geometry>&& ps) :
      box(xLength, yLength, zLength,xCorner,yCorner,zCorner,theta),
      ports(std::move(ps))
    {}

  };

 public:
  typedef std::unordered_map<switch_id, connectable*> internal_connectable_map;
  typedef std::unordered_map<node_id, connectable*> end_point_connectable_map;

 public:
  virtual ~topology();

  /**** BEGIN PURE VIRTUAL INTERFACE *****/
  /**
   * @brief Whether all network ports are uniform on all switches,
   *        having exactly the same latency/bandwidth parameters.
   *        If a 3D torus, e.g., has X,Y,Z directions exactly the same,
   *        this returns true.
   * @return
   */
  virtual bool uniform_switch_ports() const = 0;

  /**
   * @brief Whether all switches are the same
   * @return
   */
  virtual bool uniform_switches() const = 0;

  /**
   * @brief connected_outports
   *        Given a 3D torus e.g., the connection vector would contain
   *        6 entries, a +/-1 for each of 3 dimensions.
   * @param src   Get the source switch in the connection
   * @param conns The set of output connections with dst switch_id
   *              and the port numbers for each connection
   */
  virtual void connected_outports(switch_id src,
                     std::vector<topology::connection>& conns) const = 0;

  /**
   * @brief configure_individual_port_params.  The port-specific parameters
   *        will be stored in new namespaces "portX" where X is the port number
   * @param src
   * @param [inout] switch_params
   */
  virtual void configure_individual_port_params(switch_id src,
          sprockit::sim_parameters* switch_params) const = 0;

  /**
     For indirect networks, this includes all switches -
     those connected directly to nodes and internal
     switches that are only a part of the network
     @return The total number of switches
  */
  virtual switch_id num_switches() const = 0;

  virtual switch_id num_leaf_switches() const {
    return num_switches();
  }

  /**
   * @brief max_switch_id Depending on the node indexing scheme, the maximum switch id
   *  might be larger than the actual number of switches.
   * @return The max switch id
   */
  virtual switch_id max_switch_id() const = 0;

  virtual node_id num_nodes() const = 0;

  /**
   * @brief max_node_id Depending on the node indexing scheme, the maximum node id
   *  might be larger than the actual number of nodes.
   * @return The max node id
   */
  virtual node_id max_node_id() const = 0;

  /**
   * @brief get_vtk_geometry
   * @param sid
   * @return The geometry (box size, rotation, port-face mapping)
   */
  virtual vtk_switch_geometry get_vtk_geometry(switch_id sid) const;

  virtual bool is_curved_vtk_link(switch_id sid, int port) const {
    return false;
  }

  /**
   * @brief Return the maximum number of ports on any switch in the network
   * @return
   */
  virtual int max_num_ports() const = 0;

  /**
    This gives the minimal distance counting the number of hops between switches.
    @param src. The source node.
    @param dest. The destination node.
    @return The number of hops to final destination
  */
  virtual int num_hops_to_node(node_id src, node_id dst) const = 0;

  virtual switch_id endpoint_to_switch(node_id) const = 0;

  /**
   * @brief output_graphviz
   * Request to output graphviz. If file is given, output will be written there.
   * If no file is given, topology will use default path from input file.
   * If not default was given in input file, nothing will be output
   * @param file An optional file
   */
  void output_graphviz(const std::string& file = "");

  /**
   * @brief output_xyz
   * Request to output graphviz. If file is given, output will be written there.
   * If no file is given, topology will use default path from input file.
   * If not default was given in input file, nothing will be output
   * @param file An optional file
   */
  void output_xyz(const std::string& file = "");

  static void output_box(std::ostream& os,
                       const topology::vtk_box_geometry& box,
                       const std::string& color,
                       const std::string& alpha);

  static void output_box(std::ostream& os,
                       const topology::vtk_box_geometry& box);

  /**
     For a given input switch, return all nodes connected to it.
     This return vector might be empty if the
     switch is an internal switch not connected to any nodes
     @return The nodes connected to switch for injection
  */
  virtual void endpoints_connected_to_injection_switch(switch_id swid,
                          std::vector<injection_port>& nodes) const = 0;

  /**
     For a given input switch, return all nodes connected to it.
     This return vector might be empty if the
     switch is an internal switch not connected to any nodes
     @return The nodes connected to switch for ejection
  */
  virtual void endpoints_connected_to_ejection_switch(switch_id swid,
                          std::vector<injection_port>& nodes) const = 0;
  /**** END PURE VIRTUAL INTERFACE *****/

  virtual void create_partition(
    int* switch_to_lp,
    int* switch_to_thread,
    int me,
    int nproc,
    int nthread,
    int noccupied) const;

#if SSTMAC_INTEGRATED_SST_CORE
  switch_id node_to_logp_switch(node_id nid) const;

  static int nproc;
#endif


  static topology* global() {
    return main_top_;
  }

  /**
   * @brief configure_switch_params By default, almost all topologies
   *        have uniform switch parameters.
   * @param src
   * @param switch_params In/out parameter. Input is default set of params.
   *        Output is non-default unique params.
   */
  virtual void configure_nonuniform_switch_params(switch_id src,
        sprockit::sim_parameters* switch_params) const
  {
  }

  std::string label(uint32_t comp_id) const;

  virtual std::string switch_label(switch_id sid) const;

  virtual std::string node_label(node_id nid) const;

  static topology* static_topology(sprockit::sim_parameters* params);

  static void set_static_topology(topology* top){
    static_topology_ = top;
  }

  virtual cartesian_topology* cart_topology() const;

  virtual node_id node_name_to_id(std::string name) const
  {
    std::size_t pos = name.find("node");
    if (pos != 0)
      throw sprockit::input_error("topology: node name should be node<n>");
    std::string number(name,4);
    node_id id;
    try {
      id = stoi(number);
    }
    catch(...) {
      throw sprockit::input_error("topology: node name should be node<n>");
    }
    return id;
  }

  virtual switch_id switch_name_to_id(std::string name) const
  {
    std::size_t pos = name.find("switch");
    if (pos != 0)
      throw sprockit::input_error("topology: switch name should be switch<n>");
    std::string number(name,6);
    switch_id id;
    try {
      id = stoi(number);
    }
    catch(...) {
      throw sprockit::input_error("topology: switch name should be switch<n>");
    }
    return id;
  }

  virtual std::string node_id_to_name(node_id id) const
  {
    return std::string("node") + std::to_string(id);
  }

  virtual std::string switch_id_to_name(switch_id id) const
  {
    return std::string("switch") + std::to_string(id);
  }

  static void clear_static_topology(){
    if (static_topology_) delete static_topology_;
    static_topology_ = nullptr;
  }

  static std::string get_port_namespace(int port);

 protected:
  topology(sprockit::sim_parameters* params);

  static sprockit::sim_parameters* setup_port_params(
        int port, int credits, double bw,
        sprockit::sim_parameters* link_params,
        sprockit::sim_parameters* params);

  void configure_individual_port_params(int port_offset, int nports,
           sprockit::sim_parameters* params) const;

 protected:
  std::string name_;

  static topology* main_top_;

 private:
  static topology* static_topology_;
  std::string dot_file_;
  std::string xyz_file_;
};

static inline std::ostream& operator<<(std::ostream& os, const topology::xyz& v) {
  os << v.x << "," << v.y << "," << v.z;
  return os;
}

}
}



#endif
