/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_COMMON_VIS_VIS_H_INCLUDED
#define SSTMAC_COMMON_VIS_VIS_H_INCLUDED

#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/unordered.h>
#include <sprockit/factories/factory.h>

namespace sstmac {
namespace vis {

enum VIS_DIR {
  VIS_X = 0, VIS_Y, VIS_Z, VIS_NONE
};

/**
 * This is a parent class for an vis object that is passed back to
 * the simulation models to be able to manipulate
 */
class vis_obj  {
 public:

  enum VIS_COLOR {
    RED, BLUE, BLACK, GREEN, YELLOW, ORANGE, WHITE, PURPLE, CLEAR
  };

  virtual void
  set_activity(double val) = 0;

  static double clear_r;
  static double clear_g;
  static double clear_b;
  static bool clear_set;

  virtual void
  rotate(VIS_DIR dim, int degrees) {
    spkt_throw_printf(sprockit::spkt_error, "vis_obj base class - rotate is not implemented");
  }

 protected:

  vis_obj() {
    clear_set = false;
  }

  virtual
  ~vis_obj() {
  }

  virtual std::string
  to_string() const {
    return "vis_obj";
  }

};

/**
 * this is a parent class for a way of displaying a set of actions or components.
 */
class vis_engine : public sprockit::factory_type
{
 public:

  virtual void
  init() = 0;

  virtual void
  draw() = 0;

  virtual void
  draw(timestamp now) = 0;

  virtual void
  update(timestamp now) = 0;

  virtual vis_obj*
  create_cube(double x, double y, double z, double len, double degrees = 0,
              int dir = 0) = 0;

  virtual vis_obj*
  create_cylinder(double x, double y, double z, double rad, double len,
                  int dir, int degrees = 90) = 0;

  virtual vis_obj*
  create_line(double p1[3], double p2[3]) = 0;

  virtual void
  write_to_file(const std::list<vis_obj*> &objs, std::string fname) = 0;

  virtual void
  display_label(std::string text) = 0;

  virtual void
  update_label(timestamp now) = 0;

 protected:

  vis_engine() {

  }

  virtual
  ~vis_engine() {
  }

  virtual std::string
  to_string() const {
    return "vis_engine";
  }
};
DeclareFactory(vis_engine)
;

/**
 * This is a parent class for a simulation model that is associated with a vis_obj
 */
class vis_comp
{
 public:
  void
  set_vis_obj(vis_obj* o) {
    vobj_ = o;
  }

  virtual void
  set_link_vis(vis::vis_obj* vo, int dim, int dir) {

  }

  vis_obj*
  get_vis_obj() {
    return vobj_;
  }

  void
  set_vis_engine(vis_engine* e) {
    veng_ = e;
  }

  void
  vis_set_activity(double val, timestamp now);

 protected:

  vis_obj* vobj_;
  vis_engine* veng_;

  vis_comp() {

  }

  virtual
  ~vis_comp() {
  }

  virtual std::string
  to_string() const {
    return "vis_comp";
  }
};

class vis_topology
{
 public:
  typedef spkt_unordered_map<switch_id, vis::vis_comp*> vis_switch_map;

 public:
  virtual std::string
  to_string() const {
    return "vis_topology";
  }

  virtual void
  display_nodes(const vis_switch_map &switches,
                vis::vis_engine* eng,
                std::list<vis::vis_obj*> &objs) = 0;
 protected:
  vis_topology() {
  }

  virtual ~vis_topology() {}

};

/**
 * vis_display is a window that will be displayed.  simulation components
 * can become children of this class to display themselves.  they use the
 * provided vis_engine to request actions.
 */
class vis_display  {
 public:
  /**
   * Use this constructor when making a vis_display by itself
   */
  vis_display(const std::string& name, vis_engine* e,
              vis_topology::vis_switch_map m, vis_topology* t) :
    eng_(e), vismap_(m), vistop_(t), name_(name) {
    last_file_ = timestamp(0);
    filenum_ = 0;
  }

  virtual std::string
  to_string() const {
    return "vis_display";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual vis_topology::vis_switch_map
  vis_nodes() {
    return vismap_;
  }

  virtual vis_topology*
  vis_topol() {
    return vistop_;
  }

  virtual void
  vis_start(bool save_viz);

  virtual void
  vis_update(timestamp now);

  virtual void
  vis_complete() {
  }

 protected:
  /**
  * Use this constructor when inheriting from vis_display.  Make sure to
  * override vis_get_nodes() and vis_get_topology()
  */
  vis_display(const std::string& name) :
    name_(name) {
  }

  virtual
  ~vis_display() {
  }

 protected:
  vis_engine* eng_;
  vis_topology::vis_switch_map vismap_;
  vis_topology* vistop_;
  std::string name_;

  std::list<vis_obj*> objs_;

  timestamp file_interval_;
  timestamp last_file_;
  int filenum_;

};
}
}

#endif

