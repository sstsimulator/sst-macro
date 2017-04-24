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
class partition
{
  DeclareFactory(partition, parallel_runtime*)
 public:
  virtual ~partition();

  int num_switches_total() const {
    return num_switches_total_;
  }

  int lpid_for_switch(int switch_id) const {
#if SSTMAC_SANITY_CHECK
    if (switch_id >= num_switches_total_){
      spkt_throw_printf(sprockit::value_error,
          "partition::lpid_for_switch: invalid switch %d",
          switch_id);
    }
#endif
    return switch_to_lpid_[switch_id];
  }

  int thread_for_switch(int switch_id) const {
    return switch_to_thread_[switch_id];
  }

  virtual void finalize_init(){}

 protected:
  partition(sprockit::sim_parameters* params, parallel_runtime* rt);

 protected:
  int* switch_to_lpid_;

  int* switch_to_thread_;

  int num_switches_total_;

  int nthread_;

  int nproc_;

  int me_;

  parallel_runtime* rt_;

};

class serial_partition :
  public partition
{
 public:
  serial_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~serial_partition();

};


class metis_partition :
  public partition
{
 public:
  metis_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~metis_partition();

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
  topology_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~topology_partition();

 protected:
   hw::topology* fake_top_;

   int noccupied_;

};

class block_partition :
  public partition
{
 public:
  block_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~block_partition();

  void
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
  occupied_block_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~occupied_block_partition();

  virtual void
  partition_switches();

 protected:
  int occupied_switches_;
  int unoccupied_switches_;

};

}

#endif 

