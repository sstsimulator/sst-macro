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
  to_string() const override {
    return "stat_spyplot";
  }

  virtual void
  simulation_finished(timestamp end) override;

  virtual void
  dump_to_file(const std::string& froot);

  virtual void
  dump_local_data() override;

  virtual void
  dump_global_data() override;

  virtual void
  reduce(stat_collector *coll) override;

  virtual void
  global_reduce(parallel_runtime *rt) override;

  virtual void
  clear() override;

  virtual
  ~stat_spyplot() {
  }

  virtual void
  add_one(int source, int dest);

  virtual void
  add(int source, int dest, long num);

  virtual stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new stat_spyplot(params);
  }

  stat_spyplot(sprockit::sim_parameters* params) :
    max_dest_(0),
    stat_collector(params)
  {
  }

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
  stat_spyplot_png(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "stat_spyplot_png";
  }

  virtual void
  add(int source, int dest, long num) override;

  virtual void
  dump_to_file(const std::string& froot) override;

  void
  set_normalization(long max) {
    normalization_ = max;
  }

  virtual
  ~stat_spyplot_png() {
  }

  stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new stat_spyplot_png(params);
  }

 private:
  long normalization_;
  bool fill_;

};

}

#endif

