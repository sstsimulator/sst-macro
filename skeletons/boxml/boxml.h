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

#ifndef BOXML_H_INCLUDED
#define BOXML_H_INCLUDED

#include <sstmacro.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/software/process/app.h>
//#include <sstmac/software/process/key.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/sim_thread_lock.h>
#include <sstmac/common/stats/stat_histogram_fwd.h>
#include <sstmac/common/stats/stat_local_double_fwd.h>
#include <tinyxml2.h>
#include <containers.h>
#include <sprockit/unordered.h>
#include <mpi.h>
#include <sstmac/software/process/time.h>


#include <algorithm>
#include <ctime>
#include <sys/time.h>
#include <vector>
#include <list>
#include <deque>

#ifdef BOXML_HAVE_METIS
#include <metis.h>
#endif

#define BOXML_DEBUG_RANK 285
#define BOXML_DEBUG_EPOCH 35

struct epoch_check
{
  int* rank;
  int* epoch;

  void print_epoch() { printf("rank %d last epoch runnning is %d\n", *rank, *epoch); }
};

namespace lblxml {

  class box_domain;

  // typedefs
  typedef int_container_t set_t;
  typedef int_container_iter set_iter;
  typedef std::multiset<int> multiset_t;
  typedef lblxml::event event_t;
  typedef lblxml::comm comm_t;
  typedef lblxml::comp comp_t;
  typedef lblxml::reduce reduce_t;
  typedef lblxml::box box_t;
  typedef std::vector<box*> box_map_t;
  typedef std::vector<box*> box_map_iter;
  typedef std::vector<event*> event_map_t;
  typedef std::vector<int> index_to_rank_t;
  typedef std::vector<set_t> rank_to_set_t;
  typedef std::vector<std::list<int> > rank_to_list_t;
  typedef std::vector<multiset_t> rank_to_multiset_t;
  typedef std::vector<set_t>::iterator rank_to_set_iter;
  typedef std::deque<int> da_list_t;
  typedef std::vector<da_list_t> rank_to_da_list_t;
  typedef std::pair<int,int> index_box_pair_t;
  typedef std::map<index_box_pair_t,box_domain*> domain_map_t;
  typedef std::queue<index_box_pair_t> allreduce_queue_t;

  class pt2pt_message : public sumi::message
  {
  private:
    int event_index_;
  public:
    typedef sprockit::refcount_ptr<pt2pt_message> ptr;

    pt2pt_message(int index, long num_bytes) : event_index_(index),
      sumi::message(num_bytes)
    { }

    ~pt2pt_message() { }

    int event_index() { return event_index_; }
  };

  class box_domain : public sumi::communicator
  {
  private:
    const int* boxes_;
    const int* map_;
    int size_;

  public:

    box_domain() : communicator(-1) { }

    box_domain (int comm_rank, int nboxes, const int* boxes, const int* map) :
      map_(map), boxes_(boxes), size_(nboxes), communicator( comm_rank )
    { }

    ~box_domain() { }

    int
    nproc() const { return size_; }

    int
    my_box_number() const {
      return boxes_[my_comm_rank()];
    }

    int
    comm_to_global_rank(int comm_rank) const {
      int box = boxes_[comm_rank];
      int grank =  map_[box];
      return grank;
    }

    int
    global_to_comm_rank(int global_rank) const {
      std::cerr << "global_to_comm_rank() aborting\n";
      abort();
    }

  };

  // globals (that we really do want to be global)
  extern index_to_rank_t g_boxindex_to_rank; 
  extern box_map_t g_boxes;
  extern event_map_t g_events;
  extern rank_to_set_t g_rank_to_comps;
  extern rank_to_set_t g_rank_to_recvs;
  extern rank_to_set_t g_rank_to_sends;
  extern rank_to_list_t g_rank_to_allreduces;
  extern rank_to_da_list_t g_rank_to_valid_sends;
  extern rank_to_da_list_t g_rank_to_valid_comps;
  extern std::map<int, std::vector<bool> > g_reduce_to_box_running;
  extern double g_total_idle_time;
  extern double g_total_barrier_time;
  extern double g_total_compute_time;
  extern int g_active_ranks;
  extern int g_ncomp_run;
  extern int g_max_epoch_;
  extern std::map<int, std::map<int,int> > g_rank_to_epoch_count;
  extern std::map<int, std::map<int,int> > g_rank_to_epoch_done;
  extern std::map<int, double> g_rank_to_comp_t;
  extern std::map<int, double> g_rank_to_idle_t;

#ifdef BOXML_HAVE_TEST
  extern std::vector<Task*> g_task_map;
#endif

  extern std::map<int,sstmac::timestamp> g_message_begin_;

  class boxml : public sstmac::sw::app
  {
   FactoryRegister("boxml", sstmac::sw::app, boxml, "amr simulator")
  public:
    static sstmac::sim_thread_lock* event_lock;
    epoch_check checker_;

  private:

    enum synch_mode {full_synch, rank_synch, phase_asynch, full_asynch};

    int debug_, rank_, commsize_, message_factor_, repartition_size_, ncomp_,
    current_epoch_, current_epoch_events_done_;
    long vertex_scale_, fixed_vertex_;
    double compute_scale_, load_balance_tolerance_;
    std::string boxfile_, partitioning_, placement_;
    std::vector< std::string> eventfiles_;
    bool do_compute_, randomize_events_, detailed_progress_, round_robin_,
      ignore_epoch_, minimize_locks_, rank_remap_, zero_edge_weight_,
      build_graph_only_, build_chart_;
    synch_mode synch_mode_;
    static bool have_data_;
    int barrier_tag_;
    box_map_t my_boxes_;
    domain_map_t  box_domains_;
    /** stores allreduce by box number */
    allreduce_queue_t valid_allreduces_;
    int nevents_, ncomm_;
    static std::fstream bin_file_;
    static bool have_input_bin_;
    static bool have_output_bin_;
    static bool checked_bin_;
    bool xml_read_only_;
    sumi::transport* tport_;
    sstmac::stat_histogram* hist_eff_bw_;
    sstmac::stat_local_double* idle_time_;
    sstmac::stat_local_double* barrier_time_;
    sstmac::stat_local_double* compute_time_;
    sprockit::sim_parameters* params_;
    std::set<int> task_processed_;

    void
    init();

    void
    read_files();

    void
    read_binary();

    void
    process_xml(tinyxml2::XMLDocument* doc,
                std::string l1, std::string l2,
                void (*fp)(tinyxml2::XMLElement*,int));

    void
    distribute_boxes();

    void
    distribute_events();

    void
    distribute_comp(int idx, event* ev);

    void
    distribute_comm(int idx, event* ev);

    void
    distribute_allreduce(int idx, event* ev);

#ifdef BOXML_HAVE_TEST
  public:
    Task*
    my_skeleton_main();

  private:
    Task*
    get_task(int id);

    void
    add_comp_tasks(int id, int comp_id=-1);

    Task*
    build_task_graph();
#endif

    void
    fake_barrier();

    void
    fake_barrier_serial();

    void
    post_recvs();

    void
    run_loop();

    void
    finalize();

    void
    simple_event_done(int index);

    void
    clear_collective_deps(event* ev, event* evl);

    void
    collective_done(int box_number, int event_id);

    void 
    recv_boxes(int& n_events);

    bool
    send_boxes(int& n_events);

    bool
    compute_boxes(int& n_events);

    bool
    reduce(int& n_events);

    int count_events();

    int incoming_max_size();

    void post_more_recvs(int outcount);

    void post_generic_irecv (MPI_Request* request);

    void populate_listeners();

    void process_params();

    void get_params_standalone();

    void add_event_to_epoch(int rank, int epoch);

    void epoch_event_done(int epoch);
    
    int current_epoch();

    void test_for_valid_epoch();

    void barrier();

#ifdef BOXML_HAVE_METIS
    void partition_boxes();
#endif

  public:
    /// Destructor.
    virtual
    ~boxml() throw () {}

    boxml(sprockit::sim_parameters* params, sstmac::sw::software_id sid,
          sstmac::sw::operating_system* os) :
      params_(params), barrier_tag_(0), hist_eff_bw_(0), idle_time_(0),
      ncomm_(0), ncomp_(0), current_epoch_(1),
      current_epoch_events_done_(0), sstmac::sw::app(params, sid, os)
    {}

    /// Go.
    void
    skeleton_main();

  };

} // end of namespace lblxml

static enum fxn_id {
  my_sleep_id,
} test_ids;

void my_sleep(int time);

static inline double
get_real_time()
{
  //timeval t_st;
  //gettimeofday(&t_st, 0);
  //double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return sstmac_wall_time();
}

#endif