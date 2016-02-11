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

#ifndef SSTMAC_COMMON_STATS_STATS_COMMON_H_INCLUDED
#define SSTMAC_COMMON_STATS_STATS_COMMON_H_INCLUDED

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/timestamp.h>
#include <iostream>
#include <fstream>
#include <map>
#include <sprockit/unordered.h>

namespace sstmac {


/**
 * this stat_collector class keeps a spy plot
 */
class stat_spyplot :
  public stat_collector
{
 public:
  virtual std::string
  to_string() const {
    return "stat_spyplot";
  }

  virtual void
  simulation_finished(timestamp end);

  virtual void
  dump_to_file(const std::string& froot);

  virtual void
  dump_local_data();

  virtual void
  dump_global_data();

  virtual void
  reduce(stat_collector *coll);

  virtual void
  global_reduce(parallel_runtime *rt);

  virtual void
  clear();

  virtual
  ~stat_spyplot() {
  }

  virtual void
  add_one(int source, int dest);

  virtual void
  add(int source, int dest, long num);

  virtual stat_spyplot*
  clone_me(int id) const {
    stat_spyplot* cln = new stat_spyplot;
    clone_into(cln);
    cln->set_id(id);
    return cln;
  }

  virtual stat_collector*
  clone() const {
    return clone_me(-1);
  }

  stat_spyplot() : max_dest_(0) {}

 protected:
  void
  clone_into(stat_spyplot* cln) const;

 protected:
  typedef spkt_unordered_map<int, long> long_map;
  typedef spkt_unordered_map<int, long_map> spyplot_map;
  spyplot_map vals_;
  int max_dest_;

};

/**
 * this stat_collector class keeps a spy plot, and outputs it as a png
 */
class stat_spyplot_png : public stat_spyplot
{
 public:
  stat_spyplot_png() :
    fill_(false),
    normalization_(-1) {
  }

  virtual std::string
  to_string() const {
    return "stat_spyplot_png";
  }

  virtual void
  add(int source, int dest, long num);

  virtual void
  dump_to_file(const std::string& froot);

  void
  set_normalization(long max) {
    normalization_ = max;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual
  ~stat_spyplot_png() {
  }

  stat_spyplot*
  clone_me(int id) const {
    stat_spyplot_png* cln = new stat_spyplot_png;
    clone_into(cln);
    cln->set_id(id);
    return cln;
  }

  stat_collector*
  clone() const {
    return clone_me(-1);
  }

 protected:
  void
  clone_into(stat_spyplot_png* cln) const;

 protected:
  long normalization_;
  bool fill_;

};

}

#endif

