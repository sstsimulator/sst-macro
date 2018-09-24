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

#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>

#include <sprockit/fileio.h>
#include <sprockit/util.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/errors.h>

#include <cstring>

RegisterDebugSlot(partition);

#define part_debug(...) \
  debug_printf(sprockit::dbg::partition, "Rank %d: %s", me_, sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {

partition::partition(sprockit::sim_parameters* params, parallel_runtime* rt) :
  rt_(rt),
  switch_to_lpid_(nullptr),
  switch_to_thread_(nullptr)
{
  nproc_ = rt_->nproc();
  me_ = rt_->me();
  nthread_ = rt_->nthread();
}

partition::~partition()
{
}

serial_partition::~serial_partition()
{
  delete[] switch_to_thread_;
  delete[] switch_to_lpid_;
}


serial_partition::serial_partition(sprockit::sim_parameters* params, parallel_runtime* rt)
 : partition(params, rt)
{
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  hw::topology* fake_top = hw::topology::factory::get_param("name", top_params);
  int nswitches = fake_top->num_switches();
  num_switches_total_ = nswitches;
  switch_to_lpid_ = new int[nswitches];
  switch_to_thread_ = new int[nswitches];
  for (int i=0; i < nswitches; ++i){
    switch_to_lpid_[i] = 0;
    switch_to_thread_[i] = 0;
  }
  nproc_ = 1;
  me_ = 0;
  delete fake_top;
}

topology_partition::~topology_partition()
{
}

topology_partition::topology_partition(sprockit::sim_parameters* params, parallel_runtime* rt)
  : partition(params, rt)
{
  //this will need to be fixed later...
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  fake_top_ = hw::topology::factory::get_param("name", top_params);

  noccupied_ = params->get_int_param("num_occupied");

  num_switches_total_ = fake_top_->num_switches();
  switch_to_lpid_ = new int[num_switches_total_];
  switch_to_thread_ = new int[num_switches_total_];
  fake_top_->create_partition(switch_to_lpid_, switch_to_thread_,
                              me_, nproc_, nthread_, noccupied_);
}

block_partition::~block_partition()
{
}

block_partition::block_partition(sprockit::sim_parameters* params, parallel_runtime* rt)
  : partition(params, rt)
{
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  fake_top_ = hw::topology::factory::get_param("name", top_params);
  num_switches_total_ = fake_top_->num_switches();

  num_switches_total_ = fake_top_->num_switches();
  switch_to_lpid_ = new int[num_switches_total_];
  switch_to_thread_ = new int[num_switches_total_];
}

void
block_partition::partition_switches()
{
  int nworkers = nproc_ * nthread_;
  int sw_per_worker = num_switches_total_ / nworkers;
  if (num_switches_total_ % nworkers){
    ++sw_per_worker;
  }

  for (int i=0; i < num_switches_total_; ++i){
    int worker = i / sw_per_worker;
    int rank = worker / nthread_;
    int thr = worker % nthread_;
    switch_to_lpid_[i] = rank;
    switch_to_thread_[i] = thr;
  }
}

occupied_block_partition::~occupied_block_partition()
{
}

void
block_partition::finalize_init()
{
  partition_switches();
}

occupied_block_partition::occupied_block_partition(sprockit::sim_parameters* params,
                                                   parallel_runtime* rt)
  : block_partition(params, rt)
{
  occupied_switches_ = params->get_int_param("occupied_switches");
  num_switches_total_ = fake_top_->num_switches();
  unoccupied_switches_ = num_switches_total_ - occupied_switches_;

  if( occupied_switches_ < nproc_ )
    spkt_throw_printf(sprockit::input_error,
      "number of logical processes exceeds number of full switches");
  if( occupied_switches_ > num_switches_total_ )
    spkt_throw_printf(sprockit::input_error,
      "occupied_switches=%d exceeds number of switches=%d",
      occupied_switches_, num_switches_total_ );
}

void
occupied_block_partition::partition_switches()
{
  int remainder;

  // first place full switches
  int nworker = nproc_ * nthread_;
  int occupied_per_worker = occupied_switches_ / nworker;
  remainder = occupied_switches_ % nworker;
  if (remainder){
    occupied_per_worker++;
  }

  for (int i=0; i < occupied_switches_; ++i){
    int worker = i / occupied_per_worker;
    int rank = worker / nthread_;
    int thr = worker % nthread_;
    switch_to_lpid_[i] = rank;
    switch_to_thread_[i] = thr;
  }
  
  // then place empty switches
  int unoccupied_per_worker = unoccupied_switches_ / nworker;
  remainder = unoccupied_switches_ % nworker;

  for (int i=0; i < unoccupied_switches_; ++i){
    int worker = i / unoccupied_per_worker;
    int rank = worker / nthread_;
    int thr = worker % nthread_;
    switch_to_lpid_[i] = rank;
    switch_to_thread_[i] = thr;
  }
}

}
