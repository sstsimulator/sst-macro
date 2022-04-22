/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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

#ifndef SSTMAC_BACKENDS_NATIVE_SIM_PARTITION_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_SIM_PARTITION_H_INCLUDED

#include <sprockit/debug.h>
#include <sprockit/factory.h>
#include <sprockit/sim_parameters_fwd.h>

#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>

#include <sstmac/sst_core/integrated_component.h>

#include <vector>

DeclareDebugSlot(partition);

namespace sstmac {

/**
 * Class for storing all the partitions given to us by METIS.
 * The inherited boost map stores individial partitions, while
 * the parent_map_ member variable stores a flat map of switch to
 * partition.
 */
class Partition
{
 public:
  SST_ELI_DECLARE_BASE(Partition)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(SST::Params&, ParallelRuntime*)

  virtual ~Partition();

  int numSwitchesTotal() const {
    return num_switches_total_;
  }

  int lpidForSwitch(int id) const {
#if SSTMAC_SANITY_CHECK
    if (id >= num_switches_total_){
      spkt_throw_printf(sprockit::ValueError,
          "partition::lpid_for_switch: invalid switch %d",
          id);
    }
#endif
    return switch_to_lpid_[id];
  }

  int threadForSwitch(int id) const {
    return switch_to_thread_[id];
  }

  virtual void finalizeInit(SST::Params&){}

 protected:
  Partition(SST::Params& params, ParallelRuntime* rt);

 protected:
  int* switch_to_lpid_;

  int* switch_to_thread_;

  int num_switches_total_;

  int nthread_;

  int nproc_;

  int me_;

  ParallelRuntime* rt_;

};

class SerialPartition :
  public Partition
{
 public:
  SST_ELI_REGISTER_DERIVED(
    Partition,
    SerialPartition,
    "macro",
    "serial",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "partition for single rank/single thread runs")

  SerialPartition(SST::Params& params, ParallelRuntime* rt);

  ~SerialPartition() override;

};

class TopologyPartition :
  public Partition
{

 public:
  SST_ELI_REGISTER_DERIVED(
    Partition,
    TopologyPartition,
    "macro",
    "topology",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "partition derived from the topology")

  TopologyPartition(SST::Params& params, ParallelRuntime* rt);

  ~TopologyPartition() override;

 protected:
   hw::Topology* fake_top_;

   int noccupied_;

};

class BlockPartition :
  public Partition
{
 public:
  SST_ELI_REGISTER_DERIVED(
    Partition,
    BlockPartition,
    "macro",
    "block",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "partition based on simple contiguous blocks")

  BlockPartition(SST::Params& params, ParallelRuntime* rt);

  ~BlockPartition() override;

  void finalizeInit(SST::Params& params) override;

  virtual void partitionSwitches();

  hw::Topology* topology() const {
    return fake_top_;
  }

 protected:
   hw::Topology* fake_top_;

};

class OccupiedBlockPartition :
  public BlockPartition
{
 public:
  SST_ELI_REGISTER_DERIVED(
    Partition,
    OccupiedBlockPartition,
    "macro",
    "occupied_block",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "partition based on simple contiguous blocks of nodes with active procs")

  OccupiedBlockPartition(SST::Params& params, ParallelRuntime* rt);

  ~OccupiedBlockPartition() override;

  void partitionSwitches() override;

 protected:
  int occupied_switches_;
  int unoccupied_switches_;

};

}

#endif 
