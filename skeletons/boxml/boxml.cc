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

#include "boxml.h"
#include "lock_array.h"
#include <sprockit/factories/factory.h>
#include <sstmac/common/sim_thread_lock.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/stats/stat_local_double.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"boxml_boxfile",
"boxml_eventfiles",
"boxml_boxfile",
"boxml_message_factor",
"boxml_compute_scale",
"boxml_do_compute",
"boxml_debug",
"boxml_randomize_events",
"boxml_detailed_progress",
"boxml_events",
"boxml_round_robin",
"boxml_minimize_locks",
"boxml_partitioning",
"boxml_placement",
"boxml_repartition_size",
"boxml_vertex_scale",
"boxml_rank_remap",
"boxml_load_balance_tolerance",
"boxml_fixed_vertex",
"boxml_zero_edge_weight",
"boxml_build_graph_only",
"boxml_synchronization",
"boxml_binary_file",
"boxml_xml_only",
"boxml_assignment",
);

RegisterNamespaces(
"effective_bandwidths",
"polling_time",
"barrier_time",
"compute_time",
);

using namespace std;
using namespace sstmac;
using namespace sstmac::sw;
using namespace sumi;

namespace lblxml {

  bool boxml::have_data_ = false;
  bool boxml::have_input_bin_ = false;
  bool boxml::have_output_bin_ = false;
  bool boxml::checked_bin_ = false;
  std::fstream boxml::bin_file_;

  void delete_globals(){
    int nevents = g_events.size();
    for (int i=0; i < nevents; ++i){
      event* ev = g_events[i];
      if (ev) delete ev;
    }

    int nboxes = g_boxes.size();
    for (int i=0; i < nboxes; ++i){
      box* b = g_boxes[i];
      delete b;
    }
  }

  void
  boxml::get_params_standalone()
  {
    sprockit::sim_parameters* params = new sprockit::sim_parameters();
    params->parse_file("./parameters.ini", false, true);
    params_ = params;
  }

  void
  boxml::process_params()
  {
    if (eventfiles_.empty())
      params_->get_vector_param ("boxml_eventfiles", eventfiles_);
    boxfile_ =
        params_->get_param("boxml_boxfile");
    message_factor_ =
        params_->get_int_param("boxml_message_factor");
    compute_scale_ =
        params_->get_optional_double_param("boxml_compute_scale", 1.0);
    do_compute_ =
        params_->get_bool_param("boxml_do_compute");
    debug_ =
        params_->get_optional_int_param("boxml_debug",0);
    randomize_events_ =
        params_->get_bool_param("boxml_randomize_events");
    detailed_progress_ =
        params_->get_optional_bool_param("boxml_detailed_progress",false);
    nevents_ = 
        params_->get_int_param("boxml_events");
    round_robin_ =
        params_->get_optional_bool_param("boxml_round_robin",false);
    minimize_locks_ = 
        params_->get_optional_bool_param("boxml_minimize_locks",false);
    partitioning_ =
        params_->get_optional_param("boxml_partitioning", "xml");
    placement_ =
        params_->get_optional_param("boxml_placement", "xml");
    repartition_size_ =
        params_->get_optional_int_param("boxml_repartition_size",-1);
    vertex_scale_ =
        params_->get_optional_long_param("boxml_vertex_scale",1000);
    rank_remap_ =
        params_->get_optional_bool_param("boxml_rank_remap",false);
    load_balance_tolerance_ =
        params_->get_optional_double_param("boxml_load_balance_tolerance",1.05);
    fixed_vertex_ =
        params_->get_optional_long_param("boxml_fixed_vertex",0);
    zero_edge_weight_ =
        params_->get_optional_bool_param("boxml_zero_edge_weight",false);
    build_graph_only_ =
        params_->get_optional_bool_param("boxml_build_graph_only",false);

    if (params_->has_param("boxml_synchronization")) {
      string mode =
          params_->get_param("boxml_synchronization");
      if (mode == "fully_synchronous")
        synch_mode_ = full_synch;
      else if (mode == "rank_synchronous")
        synch_mode_ = rank_synch;
      else if (mode == "phase_asynchronous")
        synch_mode_ = phase_asynch;
      else if (mode == "fully_asynchronous")
        synch_mode_ = full_asynch;
      else
        spkt_throw_printf(sprockit::value_error,
          "Unrecognized option for boxml_synchronization\n");
    }
    else
      synch_mode_ = full_asynch;

#ifndef BOXML_HAVE_METIS
    if (partitioning_ == "metis" || placement_ == "metis")
      spkt_throw_printf(sprockit::value_error,
        "Not compiled with metis, can't repartition/place\n");
#endif

    if (!checked_bin_){
      if (params_->has_param("boxml_binary_file")){
        std::string bin_file = params_->get_param("boxml_binary_file");
        ifstream test(bin_file.c_str());
        if (test.good()){
          test.close();
          bin_file_.open(bin_file.c_str(), ios::in | ios::binary);
          have_input_bin_ = true;
        }  else {
          bin_file_.open(bin_file.c_str(), ios::out | ios::binary);
          have_output_bin_ = true;
        }
      }
      checked_bin_ = true;
    }

    xml_read_only_ = params_->get_optional_bool_param("boxml_xml_only", false);

    if (params_->has_namespace("effective_bandwidths")){
      sprockit::sim_parameters* stat_params = params_->get_namespace("effective_bandwidths");
      hist_eff_bw_ = test_cast(stat_histogram, stat_collector::factory::get_optional_param("type", "histogram", stat_params));

      if (!hist_eff_bw_)
        spkt_throw_printf(sprockit::value_error,
          "Effective bandwidth tracker must be histogram, %s given",
          stat_params->get_param("type").c_str());
      else
        event_manager::global->register_stat(hist_eff_bw_, nullptr);
    }

    if (params_->has_namespace("polling_time")) {
      sprockit::sim_parameters* stat_params = params_->get_namespace("polling_time");
      idle_time_ = test_cast(stat_local_double, stat_collector::factory::get_optional_param("type", "local_double", stat_params));

      if (!idle_time_)
        spkt_throw_printf(sprockit::value_error,
          "Idle time tracker type must be stat_local_double, %s given",
          stat_params->get_param("type").c_str());
    }
    if (params_->has_namespace("barrier_time")) {
      sprockit::sim_parameters* stat_params = params_->get_namespace("barrier_time");
      barrier_time_ = test_cast(stat_local_double, stat_collector::factory::get_optional_param("type", "local_double", stat_params));

      if (!idle_time_)
        spkt_throw_printf(sprockit::value_error,
          "Barrier time tracker type must be stat_local_double, %s given",
          stat_params->get_param("type").c_str());
    }
    if (params_->has_namespace("compute_time")) {
      sprockit::sim_parameters* stat_params = params_->get_namespace("compute_time");
      compute_time_ = test_cast(stat_local_double, stat_collector::factory::get_optional_param("type", "local_double", stat_params));

      if (!idle_time_)
        spkt_throw_printf(sprockit::value_error,
          "Compute time tracker type must be stat_local_double, %s given",
          stat_params->get_param("type").c_str());
    }

  }

  // instantiate the globals (that we really do want to be global)
  index_to_rank_t g_boxindex_to_rank;
  box_map_t g_boxes;
  event_map_t g_events;

  rank_to_set_t g_rank_to_comps;
  rank_to_set_t g_rank_to_recvs;
  rank_to_set_t g_rank_to_sends;
  rank_to_list_t g_rank_to_allreduces;
  rank_to_da_list_t g_rank_to_valid_sends;
  rank_to_da_list_t g_rank_to_valid_comps;
  rank_to_da_list_t g_rank_to_valid_allreduces;
  int g_ncomp_run = 0;
  std::map<int, std::map<int,int> > g_rank_to_epoch_count;
  std::map<int, std::map<int,int> > g_rank_to_epoch_done;
#ifdef BOXML_HAVE_TEST
  std::vector<Task*> g_task_map;
#endif

  std::map<int, std::vector<bool> > g_reduce_to_box_running;

  std::map<int,sstmac::timestamp> g_message_begin_;

  double g_total_idle_time = 0;
  double g_total_barrier_time = 0;
  double g_total_compute_time = 0;
  int g_active_ranks = 0;
  int g_max_epoch_ = 0;

#ifdef BOXML_HAVE_TEST
  void
  boxml::skeleton_main() {
    my_skeleton_main();
  }

  Task*
  boxml::my_skeleton_main()
  {
#else
  void
  boxml::skeleton_main()
  {
#endif

    double start;
    double stop;
    double wall_time;
    static sim_thread_lock* lock = sim_thread_lock::construct();

    if (params_ == NULL)
      get_params_standalone();
    process_params();

    // first rank to reach this point does the setup
#ifdef SSTMAC_USE_MULTITHREAD
    lock->lock();
#endif
    if (!have_data_) {
#ifdef SSTMAC_USE_MULTITHREAD
      init_event_locks();
#endif
      g_events.resize(nevents_, NULL);

      start = get_real_time();

      // the setup happens here
      if (have_input_bin_)
        read_binary();
      else
        read_files();

#ifdef BOXML_HAVE_TEST
      g_task_map.resize(nevents_, NULL);
      if (build_graph_only_) {
        return build_task_graph();
      }
#endif

      have_data_ = true;

      init();

      g_rank_to_comps.resize(commsize_);
      g_rank_to_sends.resize(commsize_);
      g_rank_to_recvs.resize(commsize_);
      g_rank_to_allreduces.resize(commsize_);
      g_rank_to_valid_allreduces.resize(commsize_);
      g_rank_to_valid_sends.resize(commsize_);
      g_rank_to_valid_comps.resize(commsize_);
      distribute_boxes();
      distribute_events();

      stop = get_real_time();
      wall_time = stop - start;
      cout << "boxml setup ran for " << wall_time << " s\n";
      cout << std::flush;
      cerr << std::flush;
    }
    else init();
#ifdef SSTMAC_USE_MULTITHREAD
    lock->unlock();
#endif

    if (xml_read_only_) {
      std::cerr << "readonly exiting\n";
#ifdef BOXML_HAVE_TEST
      return NULL;
#else
      return;
#endif
    }

    // set up deadlock checking
    if (debug_ > 0) {
      checker_.rank = &rank_;
      checker_.epoch = &current_epoch_;
      sstmac::runtime::add_deadlock_check( sstmac::new_deadlock_check(&checker_, &epoch_check::print_epoch));
    }
    if (rank_ == 0) runtime::enter_deadlock_region();
    barrier();
    barrier();
    if (rank_ == 0) {
      std::cout << g_events.size() << " total events\n";
      start = get_real_time();
    }
    barrier();

    // the actual simulation happens here
    run_loop();

    barrier();
    if (rank_ == 0) {
      stop = get_real_time();
      wall_time = stop - start;
      cout << "boxml simulation ran for " << wall_time << " s\n";
    }
    barrier();

    runtime::exit_deadlock_region();
    finalize();
    
    if (rank_==0) {
      std::cout << "Rank 0 finalized" << std::endl;
      cout << g_active_ranks << " ranks were active\n";
      std::cout << "total barrier time: " << g_total_barrier_time << "s\n"
                << "total poll time:    " << g_total_idle_time << "s\n"
                << "total compute time: " << g_total_compute_time << "s\n";
    }

    //at this point, no one should have any more events - delete stuff
    if (rank_ == 0){
      delete_globals();
    }

#ifdef BOXML_HAVE_TEST
      return NULL;
#endif

  }

  void
  boxml::init()
  {
    SSTMACBacktrace("Init");

    std::srand(std::time(0));

    comm_init();

    tport_ = sumi::sumi_api();
    runtime::add_deadlock_check(
      new_deadlock_check(tport_, &sumi::transport::deadlock_check));

    rank_ = comm_rank();
    commsize_ = comm_nproc();
    if (debug_ > 1)
      printf("Inited on rank %d of %d\n",
             rank_, commsize_); fflush(stdout);

    if (idle_time_) {
      idle_time_->set_id(rank_);
      event_manager::global->register_stat(idle_time_, nullptr);
    }
  }

  void
  boxml::finalize()
  {
    comm_finalize();
    if (debug_ > 0) cout << "rank " << rank_ << " past finalize " << now() << "\n";
  }

  void
  boxml::barrier()
  {
    sstmac::timestamp start = now();
    if (debug_ > 1) printf("rank %d starting barrier %d\n",rank_,barrier_tag_);
    comm_barrier(barrier_tag_);
    comm_collective_block(sumi::collective::barrier, barrier_tag_);
    sstmac::timestamp end = now();
    double time = (end - start).sec();
    g_total_barrier_time += time;
    ++barrier_tag_;
  }

} // end of namespace sstmac.

#ifdef BOXML_HAVE_TEST
#include <chrono>
#include <thread>

void my_sleep(int time) {
  ++lblxml::g_ncomp_run;
  std::this_thread::sleep_for(std::chrono::milliseconds(time));
}

int
boxml_standalone(int argc, char** argv)
{
  Scheduler* sch = new BasicScheduler;
  sch->init(argc,argv);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  RegisterTask(my_sleep,void,int);

  Task* root;
  if (sch->rank() == 0) {
    lblxml::boxml* me = new lblxml::boxml();
    root = me->my_skeleton_main();
    std::cout << "Task graph complete, calling run on scheduler\n";
    sch->run(root);
  }
  else {
    sch->run(NULL);
  }
  sch->stop();

  if (rank != 0)
    std::cout << "rank " << rank << " ran " << lblxml::g_ncomp_run << " computes\n";
  std::cout << "scheduler exiting normally\n";
  sch->finish();
}

RegisterTest("boxml",boxml_standalone);
#endif