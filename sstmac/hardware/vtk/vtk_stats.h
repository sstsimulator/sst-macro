/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
#ifndef sstmac_hw_vtk_stats_included_h
#define sstmac_hw_vtk_stats_included_h

#include <sstmac/common/stats/stat_collector.h>
#include <vector>
#include <queue>
#include <memory>
#include <tuple>
#include <sstmac/hardware/topology/topology.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/statapi/statfieldinfo.h>
//#include <sst/core/sst_types.h>
using namespace SST;
#endif
namespace sstmac {
namespace hw {

class Topology;

struct traffic_event {
  uint64_t time_; // progress time
  int port_;
  //this is mutable due to the nonsense that is
  //C++ sets that does not allow modifying items in the set
  //even after collision
  mutable double color_;
  int id_;

  traffic_event(uint64_t t, int port, double c, int id) :
    time_(t), port_(port), color_(c), id_(id)
  {
  }
};

#define VTK_NUM_CELLS_PER_SWITCH 7

struct vtk_link {
  uint16_t id1;
  uint16_t id2;
  uint16_t port1;
  uint16_t port2;

  vtk_link(uint16_t i1, uint16_t p1, uint16_t i2, uint16_t p2) :
    id1(i1), port1(p1), id2(i2), port2(p2)
  {
    //if (i1 > i2){ //link is identified with i1 < i2
    //  std::swap(id1,id2);
    //  std::swap(port1,port2);
    //}
  }

  uint64_t id64() const {
    uint32_t i1 = id1;
    uint32_t p1 = port1;
    uint32_t side1 = (i1 << 16) | p1;
    uint32_t i2 = id2;
    uint32_t p2 = port2;
    uint32_t side2 = (i2 << 16) | p2;

    uint64_t s1 = side1;
    uint64_t s2 = side2;
    uint64_t id = (s1 << 32) | s2;
    return id;
  }

  static vtk_link construct(uint64_t id){
    uint64_t upper32_mask = (~uint64_t(0)) << 32;
    uint32_t upper16_mask  = (~uint32_t(0)) << 16;
    uint64_t lower32_mask = (~uint64_t(0)) >> 32;
    uint32_t lower16_mask = (~(uint32_t(0))) >> 16;


    uint32_t upper32 = (upper32_mask & id) >> 32;
    uint32_t lower32 = lower32_mask & id;

    uint16_t i1 = (upper16_mask & upper32) >> 16;
    uint16_t p1 = lower16_mask & upper32;
    uint16_t i2 = (upper16_mask & lower32) >> 16;
    uint16_t p2 = lower16_mask & lower32;

    return vtk_link(i1,p1,i2,p2);
  }
};

struct vtk_port {
  uint16_t id;
  uint16_t port;
  vtk_port(uint16_t i, uint16_t p)  :
    id(i), port(p) {}

  static vtk_port construct(uint32_t id){
    uint32_t upper16_mask  = (~uint32_t(0)) << 16;
    uint32_t lower16_mask = (~(uint32_t(0))) >> 16;
    uint16_t i = (upper16_mask & id) >> 16;
    uint16_t p = (lower16_mask & id);
    return vtk_port(i,p);
  }

  uint32_t id32() const {
    uint32_t i = id;
    uint32_t p = port;
    uint32_t myid = (i << 16) | p;
    return myid;
  }
};

class StatVTK : public StatCollector
{
  FactoryRegister("vtk", stat_collector, stat_vtk)
 public:
  struct display_config {
    double idle_switch_color;
    double idle_link_color;
    double highlight_switch_color;
    double highlight_link_color;
    double bidirectional_shift;
    double min_face_color;
    double max_face_color_sum;
    double scale_face_color_sum;
    double active_face_width;
    std::string name;
    std::set<int> special_fills;
  };

  StatVTK(SST::Params& params);

  std::string toString() const override {
    return "VTK stats";
  }

  static void outputExodus(const std::string& fileroot,
      std::multimap<uint64_t, traffic_event>&& traffMap,
      const display_config& cfg,
      Topology *topo =nullptr);

  void dumpLocalData() override;

  void dumpGlobalData() override;

  void globalReduce(ParallelRuntime *rt) override;

  void clear() override;

  void collect_new_intensity(TimeDelta time, int port, double intens);

  void collect_new_color(TimeDelta time, int port, double color);

  void reduce(StatCollector *coll) override;

  void finalize(TimeDelta t) override;

  StatCollector* doClone(SST::Params& params) const override {
    return new StatVTK(params);
  }

  int id() const {
    return id_;
  }

  void configure(SwitchId sid, hw::Topology* top);

 private:
  /**
   * @brief The port_state struct
   * The VTK collection has 3 different types of quantities
   * Intensity = this is the raw input value (a double) saying what some
   *             quantity of interest is (contention delay, queue depth)
   * Level = this is a discrete quantity that maps the intensity to an integer
   *         level based upon a given set of thresholds
   * Color = this is the value (a double) that is written as a VTK state
   *         at a given timepoint. Depending on configuration,
   *         either intensity or level could be written as the color
  */
  struct port_state {
    int active_ports;
    int congested_ports;
    TimeDelta last_collection;
    TimeDelta pending_collection_start;
    int current_level;
    double accumulated_color;
    double current_color;
    double active_vtk_color;
    TimeDelta last_wait_finished;
    port_state() :
      accumulated_color(0.),
      current_level(0)
    {
    }
  };

  struct compare_events {
    bool operator()(const traffic_event& l, const traffic_event& r){
      if (l.time_ != r.time_) return l.time_ < r.time_;
      if (l.id_ != r.id_) return l.id_ < r.id_;
      return l.port_ < r.port_;
    }
  };

  std::vector<int> intensity_levels_;
  std::vector<port_state> port_states_;
  TimeDelta min_interval_;
  int id_;

  std::vector<std::pair<int,int>> filters_;

  display_config display_cfg_;

  std::set<traffic_event, compare_events> sorted_event_list_;

  std::multimap<uint64_t, traffic_event> traffic_event_map_;
  hw::Topology* top_;

  bool active_;
  bool flicker_;

};

}
}

#endif
