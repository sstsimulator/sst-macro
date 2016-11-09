/*
 *
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2010 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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

ImplementFactory(sstmac::partition);
RegisterDebugSlot(partition);

#define part_debug(...) \
  debug_printf(sprockit::dbg::partition, "Rank %d: %s", me_, sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {

SpktRegister("block", partition, block_partition);
SpktRegister("occupied_block", partition, occupied_block_partition);
SpktRegister("metis", partition, metis_partition);
SpktRegister("topology", partition, topology_partition);
SpktRegister("serial", partition, serial_partition);

partition::partition(sprockit::sim_parameters* params, parallel_runtime* rt) :
  rt_(rt),
  local_switches_(nullptr)
{
  nproc_ = rt_->nproc();
  me_ = rt_->me();
  nthread_ = rt_->nthread();
}

partition::~partition()
{
  if (local_switches_) delete[] local_switches_;
}

void
partition::init_local_switches()
{
  local_switches_ = new int[local_num_switches_];
  int idx = 0;
  for (int i=0; i < num_switches_total_; ++i){
    int lp = switch_to_lpid_[i];
    if (lp == me_){
      local_switches_[idx] = i;
      ++idx;
    }
  }

  switches_per_thread_ = local_num_switches_ / nthread_;
  if (local_num_switches_ % nthread_){
    ++switches_per_thread_;
  }
}

int
partition::thread_for_local_switch(int local_idx) const
{
  return local_idx / switches_per_thread_;
}

#if 0
void
partition::init_subsets_from_array()
{
  subsets_.resize(nproc_);
  num_switches_total_ = 0;
  for (int i=0; i < nproc_; ++i){
    int num_switches_this_lp = num_switches_per_lp_[i];
    subsets_[i] = new hw::index_subset;
    subsets_[i]->data().resize(num_switches_this_lp);
    part_debug("there are %d switches in subset %d",
        num_switches_this_lp, i);
  }
}

void
partition::build_subsets_from_array()
{
  int* tmp_counts = new int[nproc_];
  ::memset(tmp_counts, 0, nproc_ * sizeof(int));
  for (int i=0; i < num_switches_total_; ++i){
    int swid = i;
    int lp = switch_to_lpid_[swid];
    int idx = tmp_counts[lp]++;
    subsets_[lp]->data()[idx] = swid;
    //std::cout << sprockit::printf("Putting switch %d in subset %d as index %d\n", i, lp, idx);
  }
  delete[] tmp_counts;
}
#endif


serial_partition::~serial_partition()
{
  delete[] num_switches_per_lp_;
  delete[] switch_to_lpid_;
  for (int i=0; i < subsets_.size(); ++i){
    delete subsets_[i];
  }
}


serial_partition::serial_partition(sprockit::sim_parameters* params, parallel_runtime* rt)
 : partition(params, rt)
{
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  hw::topology* fake_top = hw::topology_factory::get_param("name", top_params);
  int nswitches = fake_top->num_switches();
  num_switches_per_lp_ = new int[1];
  num_switches_per_lp_[0] = nswitches;
  num_switches_total_ = nswitches;
  local_num_switches_ = nswitches;
  switch_to_lpid_ = new int[nswitches];
  for (int i=0; i < nswitches; ++i){
    switch_to_lpid_[i] = 0;
  }
  init_local_switches();
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
    rt_->bcast(num_switches_per_lp_, nproc_ * sizeof(int), root);
    rt_->bcast(switch_to_lpid_, num_switches_total_ * sizeof(int), root);
  }
  else {
    int root = 0;
    num_switches_per_lp_ = new int[nproc_];
    rt_->bcast(num_switches_per_lp_, nproc_ * sizeof(int), root);
    switch_to_lpid_ = new int[num_switches_total_];
    rt_->bcast(switch_to_lpid_, num_switches_total_ * sizeof(int), root);
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
  num_switches_per_lp_ = new int[num_subsets];
  subsets_.resize(num_subsets);
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
    subsets_[lp] = subset;
    num_switches_per_lp_[lp] = num_nodes_lp;
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
  fake_top_ = hw::topology_factory::get_param("name", top_params);

  noccupied_ = params->get_int_param("num_occupied");

  num_switches_per_lp_ = new int[nproc_];
  num_switches_total_ = fake_top_->num_switches();
  switch_to_lpid_ = new int[num_switches_total_];
  local_switch_to_thread_ = new int[num_switches_total_];
  fake_top_->create_partition(num_switches_per_lp_, switch_to_lpid_,
    local_switch_to_thread_, local_num_switches_,
    me_, nproc_, nthread_, noccupied_);
  init_local_switches();
}

block_partition::~block_partition()
{
}

block_partition::block_partition(sprockit::sim_parameters* params, parallel_runtime* rt)
  : partition(params, rt)
{
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  fake_top_ = hw::topology_factory::get_param("name", top_params);
  num_switches_total_ = fake_top_->num_switches();

  num_switches_total_ = fake_top_->num_switches();
  switch_to_lpid_ = new int[num_switches_total_];
  num_switches_per_lp_ = new int[nproc_];
  for(int i=0; i<nproc_; ++i) num_switches_per_lp_[i] = 0;
}

void
block_partition::partition_switches()
{
  int sw_per_lp = num_switches_total_ / nproc_;
  if (num_switches_total_ % nproc_){
    ++sw_per_lp;
  }

  for (int i=0; i < num_switches_total_; ++i){
    int lp = i / sw_per_lp;
    switch_to_lpid_[i] = lp;
    num_switches_per_lp_[lp]++;
  }

  int remainder = num_switches_total_ - sw_per_lp*me_;
  remainder = std::max(0, remainder);
  local_num_switches_ = std::min(remainder, sw_per_lp);
}

occupied_block_partition::~occupied_block_partition()
{
}

void
block_partition::finalize_init()
{
  partition_switches();
  init_local_switches();
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
  occupied_per_lp_ = occupied_switches_ / nproc_;
  remainder = occupied_switches_ % nproc_;
  if (remainder){
    occupied_per_lp_++;
  }

  for (int i=0; i < occupied_switches_; ++i){
    int lp = i / occupied_per_lp_;
    switch_to_lpid_[i] = lp;
    num_switches_per_lp_[lp]++;
  }
  
  // then place empty switches
  unoccupied_per_lp_ = unoccupied_switches_ / nproc_;
  remainder = unoccupied_switches_ % nproc_;

  for (int i=0; i < unoccupied_switches_; ++i){
    int lp = i / unoccupied_per_lp_;
    switch_to_lpid_[i] = lp;
    num_switches_per_lp_[lp]++;
  }

  if (me_ < (nproc_ - 1)) {
    my_num_occupied_ = occupied_per_lp_;
    my_num_unoccupied_ = unoccupied_per_lp_;
  } 
  else {
    my_num_occupied_ = occupied_switches_ - occupied_per_lp_ * (nproc_-1);
    my_num_unoccupied_ = unoccupied_switches_ - unoccupied_per_lp_ * (nproc_-1);
  }

  local_num_switches_ = my_num_occupied_ + my_num_unoccupied_;

  occupied_per_thread_ = my_num_occupied_ / nthread_;
  if (occupied_per_thread_ % nthread_){
    ++occupied_per_thread_;
  }

  unoccupied_per_thread_ = my_num_unoccupied_ / nthread_;
  if (unoccupied_per_thread_ % nthread_){
    ++unoccupied_per_thread_;
  }
}

int
occupied_block_partition::thread_for_local_switch(int local_idx) const
{
  if (local_idx < my_num_occupied_) {
    return local_idx / occupied_per_thread_;
  }
  else {
    int offset_idx = local_idx - my_num_occupied_;
    return offset_idx / unoccupied_per_thread_;
  }
}

}




