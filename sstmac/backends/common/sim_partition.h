/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2010 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_BACKENDS_NATIVE_SIM_PARTITION_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_SIM_PARTITION_H_INCLUDED

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters_fwd.h>

#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>

#include <vector>

DeclareDebugSlot(partition);

namespace sstmac {

/**
 * Class for storing all the partitions given to us by METIS.
 * The inherited boost map stores individial partitions, while
 * the parent_map_ member variable stores a flat map of switch to
 * partition.
 */
class partition :
  public sprockit::factory_type
{

 public:
  virtual std::string
  to_string() const {
    return "partition";
  }

  virtual ~partition();

  hw::index_subset*
  subset(int lpid) const {
    if(lpid >= subsets_.size()) {
      spkt_throw_printf(sprockit::value_error,
                       "partition::get_partition: invalid lpid %d requested for parallel run",
                       lpid);
    }
    return subsets_[lpid];
  }

  int*
  num_switches_per_lp() {
    return num_switches_per_lp_;
  }

  int
  local_switch(int idx) const {
    return local_switches_[idx];
  }

  int*
  local_switches() {
    return local_switches_;
  }

  int*
  switch_to_lpid() {
    return switch_to_lpid_;
  }

  int
  num_switches_total() const {
    return num_switches_total_;
  }

  int
  local_num_switches() const {
    return local_num_switches_;
  }

  virtual void
  init_param1(parallel_runtime* rt);

  int
  lpid_for_switch(int switch_id) const {
#if SSTMAC_SANITY_CHECK
    if (switch_id >= num_switches_total_){
      spkt_throw_printf(sprockit::value_error,
          "partition::lpid_for_switch: invalid switch %d",
          switch_id);
    }
#endif
    return switch_to_lpid_[switch_id];
  }

  virtual int
  thread_for_local_switch(int local_idx) const;

 protected:
  partition();

  void init_local_switches();

 protected:
  int* switch_to_lpid_;

  int* num_switches_per_lp_;

  int* local_switches_;

  int num_switches_total_;

  int local_num_switches_;

  int switches_per_thread_;

  int nthread_;

  int nproc_;

  int me_;

  parallel_runtime* rt_;

  std::vector<hw::index_subset*> subsets_;


};

DeclareFactory1InitParam(partition, parallel_runtime*);

class serial_partition :
  public partition
{
 public:
  serial_partition();

  virtual ~serial_partition();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

};


class metis_partition :
  public partition
{
 public:
  virtual ~metis_partition();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

 protected:
  void
  read_partition(const std::string &partfilename, int nproc);

 protected:
  hw::interconnect* fake_ic_;

};

class topology_partition :
  public partition
{
 public:
  topology_partition();

  virtual ~topology_partition();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  virtual int
  thread_for_local_switch(int local_idx) const {
    return local_switch_to_thread_[local_idx];
  }

 protected:
   hw::topology* fake_top_;

   int* local_switch_to_thread_;

   int noccupied_;

};

class block_partition :
  public partition
{
 public:
  block_partition();

  virtual ~block_partition();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  virtual void
  partition_switches();

 protected:
   hw::topology* fake_top_;

};

class occupied_block_partition :
  public block_partition
{
 public:
  occupied_block_partition();

  virtual ~occupied_block_partition();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  partition_switches();

  virtual void
  finalize_init();

  int
  thread_for_local_switch(int local_idx) const;

 protected:
  int occupied_switches_;
  int unoccupied_switches_;
  int occupied_per_lp_;
  int unoccupied_per_lp_;
  int my_num_occupied_;
  int my_num_unoccupied_;
  int occupied_per_thread_;
  int unoccupied_per_thread_;

};

}

#endif 

