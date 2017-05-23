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

#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/index_subset.h>

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

metis_partition::~metis_partition()
{
}

metis_partition::metis_partition(sprockit::sim_parameters* params, parallel_runtime* rt)
  : partition(params, rt)
{


  spkt_throw(sprockit::unimplemented_error, "metis_partition::finalize_init");
  std::string graph_file = "partition.metis";
  std::string part_file = sprockit::printf("partition.metis.part.%d", nproc_);


  // Read in partitions
  if (me_ == 0){
#if !SSTMAC_INTEGRATED_SST_CORE
    //hw::switch_interconnect* fab = safe_cast(hw::switch_interconnect, fake_ic_);
    //fab->write_graph_file(graph_file);
#else
    assert(false); // not implemented
#endif

    system("rm -f partition.metis.part.*");

    std::string cmd = sprockit::printf("gpmetis -contig %s %d", graph_file.c_str(), nproc_);
    int rc = system(cmd.c_str());
    if (rc != 0){
      spkt_throw_printf(sprockit::illformed_error,
        "METIS failed to construct a parallel partition!");
    }

    read_partition(part_file, nproc_);

    int root = 0;
    /** BCAST both bits of information */
    rt_->bcast(switch_to_lpid_, num_switches_total_ * sizeof(int), root);
    rt_->bcast(switch_to_thread_, num_switches_total_ * sizeof(int), root);
  }
  else {
    int root = 0;
    switch_to_lpid_ = new int[num_switches_total_];
    rt_->bcast(switch_to_lpid_, num_switches_total_ * sizeof(int), root);
    switch_to_thread_ = new int[num_switches_total_];
    rt_->bcast(switch_to_thread_, num_switches_total_ * sizeof(int), root);
  }

  fake_ic_ = 0;
}

void
metis_partition::read_partition(const std::string &partfilename, int nproc)
{
  nproc_ = nproc;

  std::string thef = sprockit::trim_str(partfilename);
  part_debug("reading partition file %s", thef.c_str());

  std::ifstream is;
  sprockit::SpktFileIO::open_file(is, thef);

  std::vector<std::list<int> > partitions;
  partitions.resize(nproc);

  // Reads in METIS partition files.
  int num_nodes = 0;
  int max_lp = 0;
  if (is.is_open()) {
    int i = 0;
    std::string line;
    std::getline(is, line);
    while (is.good()) {
      sprockit::trim_str(line);
      // Insert switch into partition.
      int lp = atoi(line.c_str());
      partitions[lp].push_back(i);
      part_debug("putting switch %d in subset %d", i, lp);
      max_lp = std::max(lp, max_lp);
      ++num_nodes;
      i++;
      std::getline(is, line);
    }

    is.close();
  }
  else {
    spkt_throw_printf(sprockit::value_error,
                     "parallel::partition::read_partition - could not open file %s \n",
                     partfilename.c_str());
  }

  //now that we have sorted all the partition data, create the array
  int num_subsets = max_lp+1;
  switch_to_lpid_ = new int[num_nodes];
  num_switches_total_ = 0;
  for (int lp=0; lp <= max_lp; ++lp){
    const std::list<int>& node_list = partitions[lp];
    int num_nodes_lp = node_list.size();
    part_debug("there are %d switches in subset %d", num_nodes_lp, lp);
    hw::index_subset* subset = new hw::index_subset;
    std::vector<int>& subset_nodes = subset->data();
    subset_nodes.resize(num_nodes_lp);
    int idx = 0;
    for (int swid : node_list){
      switch_to_lpid_[swid] = lp;
      subset_nodes[idx] = swid;
      ++idx;
    }
    num_switches_total_ += num_nodes_lp;
  }

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