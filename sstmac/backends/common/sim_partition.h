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
  FactoryRegister("serial", partition, serial_partition)
 public:
  serial_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~serial_partition();

};


class metis_partition :
  public partition
{
  FactoryRegister("metis", partition, metis_partition)
 public:
  metis_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~metis_partition();

 protected:
  void read_partition(const std::string &partfilename, int nproc);

 protected:
  hw::interconnect* fake_ic_;

};

class topology_partition :
  public partition
{
  FactoryRegister("topology", partition, topology_partition)

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
  FactoryRegister("block", partition, block_partition)
 public:
  block_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~block_partition();

  void finalize_init();

  virtual void partition_switches();

  hw::topology* top() const {
    return fake_top_;
  }

 protected:
   hw::topology* fake_top_;

};

class occupied_block_partition :
  public block_partition
{
  FactoryRegister("occupied_block", partition, occupied_block_partition)
 public:
  occupied_block_partition(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual ~occupied_block_partition();

  virtual void partition_switches();

 protected:
  int occupied_switches_;
  int unoccupied_switches_;

};

}

#endif 