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
#include <sstmac/common/config.h>
#include <sstmac/software/process/operating_system.h>

#include <cstdlib>

using namespace std;
using namespace tinyxml2;
using namespace sumi;

#define sanity_index(i,v) \
  if (i >= v.size()){ \
    spkt_throw_printf(sprockit::value_error, \
      "trying to access index %d, max is %d", \
      i, v.size()); \
  }

#undef sanity_index
#define sanity_index(i,v)

#ifdef BOXML_HAVE_METIS
#include <aphid.h>
#endif

namespace lblxml
{

#ifdef BOXML_HAVE_METIS
  bool compare(const pair<int, idx_t>&i, const pair<int, idx_t>&j)
  {
    return i.second > j.second;
  }
#endif

  int count_xml(tinyxml2::XMLDocument* doc,
                const string& level1, const string& level2);
  void read_box(XMLElement* element, int debug);
  void read_comm_event(XMLElement* element, int debug);
  void read_coll_event(XMLElement* element, int debug);
  void read_comp_event(XMLElement* element, int debug);

  static lock_array locks;

  void
  init_event_locks()
  {
    locks.init(100);
  }

  event*
  acquire_event(int index)
  {
#ifdef SSTMAC_USE_MULTITHREAD
    locks.lock(index);
#endif
    return g_events[index];
  }
  
  void
  release_event(int index)
  {
#ifdef SSTMAC_USE_MULTITHREAD
    locks.unlock(index);
#endif
  }



  void boxml::read_binary() {
    bin_file_.seekg(0, bin_file_.end);
    long size = bin_file_.tellg();
    printf("Reading all the data from a file of size %d\n", (int)size);
    bin_file_.seekg(0, bin_file_.beg);
    char* alldata = new char[size];
    bin_file_.read(alldata, size);
    sprockit::serializer ser;
    ser.start_unpacking(alldata, size);
    int n_boxes = 0;
    ser & n_boxes;
    g_boxes.resize(n_boxes);
    g_boxindex_to_rank.resize(n_boxes);
    for (int i=0; i < n_boxes; ++i){
      box* b;
      ser & b;
      g_boxes[b->index()] = b;
    }

    int n_events = 0;
    ser & n_events;
    int n_events_nonnull = 0;
    ser & n_events_nonnull;

    g_events.resize(n_events);
#ifdef BOXML_HAVE_TEST
    g_task_map.resize(n_events,NULL);
#endif
    //events are not dense - so might be a lot of nulls
    for (int i=0; i < n_events_nonnull; ++i){
      event* ev;
      ser & ev;
      g_events[ev->index()] = ev;
      if (ev->event_type() == event::collective){
        static_cast<reduce_t*>(ev)->compute_box_array();
      }
    }

    bin_file_.close();

    populate_listeners();
  }

  void boxml::read_files() {

    int error;

    // read in boxes
    std::cout << "boxfile name: " << boxfile_.c_str() << "\n";
    tinyxml2::XMLDocument boxdoc;
    error = boxdoc.LoadFile( boxfile_.c_str() );
    if (error != XML_NO_ERROR) {
      cerr << "Error " << error << " while loading boxfile\n";
      abort();
    }

    if (debug_ > 2) boxdoc.Print();
    int n_boxes = count_xml(&boxdoc, "boxes", "box");
    std::cout << "Reading in " << n_boxes << " boxes" << std::endl;
    g_boxindex_to_rank.resize( n_boxes );
    g_boxes.resize( n_boxes );
    process_xml(&boxdoc, "boxes", "box", read_box);

    // read in events
    std::vector< std::string >::iterator it = eventfiles_.begin();
    for(; it != eventfiles_.end(); ++it) {
      std::cout << "Starting to load eventfile " << *it << std::endl;
      tinyxml2::XMLDocument eventdoc;
      error = eventdoc.LoadFile( (*it).c_str() );
      if (error != XML_NO_ERROR) {
        cerr << "Error " << error << " while loading eventfile\n";
        abort();
      }
      if (debug_ > 2) eventdoc.Print();
      std::cout << "Done loading eventfile " << *it << std::endl;
      std::cout << "Processing events" << std::endl;

      // not all traces have collectives
      int n_coll = 0;
      XMLElement* e1 = eventdoc.FirstChildElement("events");
      XMLElement* e2;
      if (e1)
        e2 = e1->FirstChildElement("coll");
      /**
      if (e2)
        n_coll = count_xml(&eventdoc, "events", "coll");


      int n_comm = count_xml(&eventdoc, "events", "comm");
      int n_comp = count_xml(&eventdoc, "events", "comp");
      int nevents_test = n_coll + n_comm + n_comp;
      if (nevents_test > nevents_){
        spkt_throw(sprockit::value_error,
          "insufficient number of event spots in vector: given %d, have %d",
          nevents_, nevents_test);
      }
      //g_events.resize( n_coll + n_comm + n_comp );
      */

      process_xml(&eventdoc, "events", "comp", read_comp_event);
      process_xml(&eventdoc, "events", "comm", read_comm_event);
      if (e2){
        process_xml(&eventdoc, "events", "coll", read_coll_event);
      }
    }

    if (have_output_bin_){
      sprockit::serializer ser;
      //figure out how big the xml is TODO
      int tmp_size = 1e8;
      int commit_size = 9e7;
      char* ser_buffer = new char[tmp_size];
      ser.start_packing(ser_buffer, tmp_size);
      int nb = g_boxes.size();
      ser & nb;
      for (int i=0; i < nb; ++i){
        ser & g_boxes[i];
        if (ser.size() > commit_size){
          printf("Commit up to box %d of %d to disk\n", i , nb);
          bin_file_.write(ser_buffer, ser.size());
          ser.start_packing(ser_buffer, tmp_size);
        }
      }

      int ne = g_events.size();
      ser & ne;
      int ne_nonnull = 0;
      for (int i=0; i < ne; ++i){
        if (g_events[i]) ++ne_nonnull;
      }

      ser & ne_nonnull;
      for (int i=0; i < ne; ++i){
        event* ev = g_events[i];
        if (ev){ ser & ev;
          if (ser.size() > commit_size){
            printf("Commit up to event %d of %d to disk\n", i , ne);
            bin_file_.write(ser_buffer, ser.size());
            ser.start_packing(ser_buffer, tmp_size);
          }
        }
      }

      if (ser.size())
        bin_file_.write(ser_buffer, ser.size());

      bin_file_.close();
    }

    srand(0);
    if (partitioning_ != "xml") {
      if (partitioning_ == "sequential") {

        int sloc = 0;
        for (int i=0; i < g_boxes.size(); ++i) {
          g_boxes[i]->change_loc(sloc);
          ++sloc;
          if (sloc == commsize_) sloc = 0;
        }
      }
#ifdef BOXML_HAVE_METIS
      else if (partitioning_ == "metis" || partitioning_ == "knapsack")  {
        partition_boxes();
      }
#endif
      else if (partitioning_ == "random") {
        std::cerr << "partitioning boxes randomly\n";
        std::vector<int> ids;
        for (int i=0; i < g_boxes.size(); ++i)
          ids.push_back(i);
        std::random_shuffle(ids.begin(),ids.end());
        int new_loc=0;
        for (int i=0; i < g_boxes.size(); ++i) {
          g_boxes[ids.back()]->change_loc(new_loc);
          ids.pop_back();
          ++new_loc;
          if(new_loc == commsize_) new_loc = 0;
        }
      }
      else {
        spkt_throw_printf(sprockit::value_error,
                          "invalid boxml_assignment value");
      }
    }

    if (placement_ == "metis" || placement_ == "random") {
      std::vector<int> placement_map;
      placement_map.resize(commsize_);
      if (placement_ == "metis") {
#ifdef BOXML_HAVE_METIS
        rank_placer::partitioner* part = new rank_placer::partitioner();
        part->do_rank_placer(NULL,NULL,params_,&placement_map);
#else
        std::cerr << "metis not available!\n";
        abort();
#endif
      }
      else if (placement_ == "random") {
        std::random_shuffle(placement_map.begin(),placement_map.end());
      }
      std::cerr << "placement (" << placement_map.size() << ")\n";
      for (int i=0; i < placement_map.size(); ++i) {
        std::cerr << placement_map[i] << "\n";
      }
      for (int i=0; i < g_boxes.size(); ++i) {
        int loc = g_boxes[i]->loc();
        g_boxes[i]->change_loc(placement_map[loc]);
      }
    }

    populate_listeners();
  }

  void boxml::populate_listeners() {
    for (int i=0; i < g_events.size(); ++i) {
      event* child_ev = g_events[i];
      if (child_ev == NULL) //can have gaps
        continue;
      int_container_t& dep = child_ev->get_dep();
      for(int_container_iter dep_it = dep.begin();
        dep_it != dep.end(); ++dep_it){
        int index = *dep_it;
        event* parent_ev = g_events[index];
        switch(parent_ev->event_type()){
          case event::collective:
          {
            reduce_t* coll = static_cast<reduce_t*>(parent_ev);
            if (child_ev->event_type() == event::computation){
              comp_t* listener = static_cast<comp_t*>(child_ev);
              int box_number = listener->at();
              coll->add_listener(box_number,i);
            }
            else if (child_ev->event_type() == event::collective ){
              reduce_t* listener = static_cast<reduce_t*>(child_ev);
              const int* boxes = listener->box_array();
              for(int b=0; b < listener->nboxes(); ++b) {
                coll->add_listener(boxes[b],i);
              }
            }
            else if (child_ev->event_type() == event::pt2pt){
              comm_t* listener = static_cast<comm_t*>(child_ev);
              int box_number = listener->from();
              coll->add_listener(box_number,i);
            }
            else
              spkt_throw_printf(sprockit::value_error,
                "adding ? listener %d to allreduce %d",
                child_ev->index(), parent_ev->index());
            break;
          }
          default:
          {
            simple_event* sev = static_cast<simple_event*>(parent_ev);
            sev->add_listener(i);
            break;
          }
        }
      }
    }
  }

  void boxml::distribute_boxes() {
    SSTMACBacktrace("distribute boxes");
    rank_to_set_t rank_to_boxes;
    rank_to_boxes.resize(commsize_);
    for (int i=0; i < g_boxes.size(); ++i) {
      int ind = g_boxes[i]->index();
      int rnk = g_boxes[i]->loc();
      if (debug_ > 1) rank_to_boxes[rnk].insert(ind);
      g_boxindex_to_rank[ind] = rnk;
      if (rnk >= commsize_ && partitioning_ != "metis"){
        spkt_throw_printf(sprockit::value_error,
          "box %d maps to rank %d, which is greater than given number of ranks %d - check launch_app params",
          ind, rnk, commsize_);
      }
    }

    if (debug_ > 1) {
      for (int i=0; i < rank_to_boxes.size(); ++i) {
        cout << "rank " << i << " has boxes: ";
        for (set_t::iterator box_it = rank_to_boxes[i].begin();
             box_it != rank_to_boxes[i].end(); ++box_it) {
          cout << *box_it << " ";
        }
        cout << "\n";
      }
      cout << "\n";
    }
  }


  void boxml::distribute_comm(int i, event* ev){
    comm_t* comm = static_cast<comm_t*>(ev);
    int to = g_boxindex_to_rank[comm->to()];
    int from = g_boxindex_to_rank[comm->from()];
    fflush(stdout);
    sanity_index(from, g_rank_to_sends)
    sanity_index(to, g_rank_to_sends)
    sanity_index(from, g_rank_to_valid_sends)
    if (to != from) {
      g_rank_to_sends[from].insert(i);
      g_rank_to_recvs[to].insert(i);
      if (synch_mode_ < phase_asynch) {
        add_event_to_epoch(to,comm->epoch());
        add_event_to_epoch(from,comm->epoch());
        if (debug_ > 0 && (to == BOXML_DEBUG_RANK || from == BOXML_DEBUG_RANK) && comm->epoch() == BOXML_DEBUG_EPOCH )
          printf("rank %d added event %d to epoch %d\n", BOXML_DEBUG_RANK, i, BOXML_DEBUG_EPOCH);
      }
      if (comm->n_dep() == 0) {
        g_rank_to_valid_sends[from].push_back(i);
      }
    }
  }

  void boxml::distribute_comp(int i, event* ev){
    comp_t* comp = static_cast<comp_t*>(ev);
    int at = g_boxindex_to_rank[comp->at()];
    fflush(stdout);
    sanity_index(at, g_rank_to_comps)
    sanity_index(at, g_rank_to_valid_comps)
    g_rank_to_comps[at].insert(i);
    if (synch_mode_ < full_asynch) {
      add_event_to_epoch(at,comp->epoch());
      if (debug_ > 0 && at == BOXML_DEBUG_RANK && comp->epoch() == BOXML_DEBUG_EPOCH )
          printf("rank %d added event %d to epoch %d\n", BOXML_DEBUG_RANK, i, BOXML_DEBUG_EPOCH);
    }
    if (comp->n_dep() == 0) {
      g_rank_to_valid_comps[at].push_back(i);
    }
  }

  void boxml::distribute_allreduce(int i, event* ev){
    reduce_t* comm = static_cast<reduce_t*>(ev);
    box_to_domain_rank_map& team_map = comm->get_team();
    box_to_domain_rank_map::iterator it, end = team_map.end();
    for (it=team_map.begin(); it != end; ++it){
      int box_number = it->first;
      int rank = g_boxindex_to_rank[box_number];
      g_rank_to_allreduces[rank].push_back(i);
      if (synch_mode_ < phase_asynch) {
        add_event_to_epoch(rank,comm->epoch());
        if (debug_ > 0 && rank == BOXML_DEBUG_RANK && comm->epoch() == BOXML_DEBUG_EPOCH )
          printf("rank %d added event %d to epoch %d\n", BOXML_DEBUG_RANK, i, BOXML_DEBUG_EPOCH);
      }
    }
    // assume there are no valid allReduces to start off
  }

  void boxml::distribute_events() {
    SSTMACBacktrace("distribute events");

    // "i" is the same as the index
    for (int i=0; i < g_events.size(); ++i) {
      event* ev = g_events[i];
      if (ev == NULL)
        continue;

      fflush(stdout);
      switch(ev->event_type()){
        case event::collective:
          distribute_allreduce(i, ev);
          //g_reduce_to_box_running[i].assign(g_boxes.size(),0);
          //printf("setting coll %d running to false\n",i);
          //g_reduce_to_box_running[i] = std::vector<bool>();
          g_reduce_to_box_running[i].resize(g_boxes.size());
          for (int j=0; j < g_boxes.size(); ++j) {
            g_reduce_to_box_running[i][j] = false;
            //std::cout << g_reduce_to_box_running[i][j] << std::endl;
          }
          for (int j=0; j < g_boxes.size(); ++j) {
            //std::cout << g_reduce_to_box_running[i][j] << std::endl;
            //printf("box %d: %d\n",j,g_reduce_to_box_running[i][j]);
          }
          break;
        case event::pt2pt:
          distribute_comm(i, ev);
          break;
        case event::computation:
          distribute_comp(i, ev);
          break;
        case event::none:
          spkt_abort_printf("Invalid event: has none type");
      }
    }
  }

  /**
   * @brief boxml::clear_collective_deps
   * This should only get called if next_ev is a collective event.
   * @param done_ev  Completed event, either a computation or a collective.
   * @param next_ev  Listening event, should always be a collective.
   */
  void
  boxml::clear_collective_deps(event* done_ev, event* next_ev)
  {
    // should check event type of next_ev
    int done_event_id = done_ev->index();
    int next_event_id = next_ev->index();
    reduce_t* next_reduce = static_cast<reduce_t*>(next_ev);
    next_reduce->remove_dep(done_event_id);
    if (debug_ > 1)
      printf("rank %d reduce event %d to %d deps from %d\n", 
             rank_, next_event_id, next_reduce->n_dep(), done_event_id);
    std::vector<int> check_boxes;
    std::vector<bool> check_valids;

    // for completed collective: check each box associated with done_ev
    // if done box is on this rank and is member of next_team
    // then next_reduce might be valid
    if (done_ev->event_type() == event::collective) {
      reduce_t* done_reduce = static_cast<reduce_t*>(done_ev);
      const int* done_boxes = done_reduce->box_array();
      box_to_domain_rank_map& next_team = next_reduce->get_team();
      for (int i=0; i < done_reduce->nboxes(); ++i) {
        if (g_boxindex_to_rank[done_boxes[i]] == rank_ &&
            next_team.find(done_boxes[i]) != next_team.end()) {
          check_boxes.push_back(done_boxes[i]);
          check_valids.push_back(true);
        }
      }
    }

    // for completed computation: function should only be called if
    // next_ev is a listener, so done_ev might now be valid
    else if (done_ev->event_type() == event::computation) {
      comp_t* done_comp = static_cast<comp_t*>(done_ev);
      check_boxes.push_back(done_comp->at());
      check_valids.push_back(true);
    }

    // must check to see if any other dependencies are blocking
    // a done box before putting it on valid list
    int_container_t& deps = next_reduce->get_dep();
    for(int_container_iter dep_it = deps.begin();
      dep_it != deps.end(); ++dep_it){
      event* dep_ev = g_events[*dep_it];
      for( int bi=0; bi < check_boxes.size(); ++bi) {
        int done_at = check_boxes[bi];
        if (dep_ev->event_type() == event::computation) {
          comp_t* comp_ev = static_cast<comp_t*>(dep_ev);
          int at = comp_ev->at();
          if (at == done_at)
            check_valids[bi] = false;
        }
        if (dep_ev->event_type() == event::collective) {
          reduce_t* dep_reduce = static_cast<reduce_t*>(dep_ev);
          const int* boxes = dep_reduce->box_array();
          for (int i=0; i < dep_reduce->nboxes(); ++i)
            if (boxes[i] == done_at)
              check_valids[bi] = false;
        }
      }
    }

    for (int i=0; i < check_boxes.size(); ++i) {
      if (check_valids[i] == true) {
        index_box_pair_t coll(next_event_id,check_boxes[i]);
        if (!g_reduce_to_box_running[next_event_id][check_boxes[i]]) {
          g_reduce_to_box_running[next_event_id][check_boxes[i]] = true;
          valid_allreduces_.push(coll);
        }
      }
    }

  }

  void
  boxml::collective_done(int box_number, int event_id)
  {
    event* done_ev = g_events[event_id];
    reduce_t* done_red = static_cast<reduce_t*>(done_ev);
    reduce_t::listener_iterator it, end = done_red->listener_end(box_number);
    for (it=done_red->listener_begin(box_number); it != end; ++it)
    {
      int next_event_id = *it;

      event_t* next_ev;
      if (!minimize_locks_)
        next_ev = acquire_event(next_event_id);
      else
        next_ev = g_events[next_event_id];

      if (next_ev->event_type() == event::computation) {
        comp_t* comp_ev = static_cast<comp_t*>(next_ev);
        comp_ev->remove_dep(event_id);
        if (debug_ > 1)
          printf("rank %d event %d to %d deps from %d\n", 
                 rank_, next_event_id, comp_ev->n_dep(), event_id);
        if (comp_ev->n_dep() == 0) {
          g_rank_to_valid_comps[rank_].push_back(next_event_id);
        }
      }
      else if (next_ev->event_type() == event::pt2pt) {
        comm_t* comm_ev = static_cast<comm_t*>(next_ev);
        comm_ev->remove_dep(event_id);
        if (debug_ > 1)
          printf("rank %d event %d to %d deps from %d\n",  
                 rank_, next_event_id, comm_ev->n_dep(), event_id);
        if (comm_ev->n_dep() == 0) {
          g_rank_to_valid_sends[rank_].push_back(next_event_id);
        }
      }
      else if (next_ev->event_type() == event::collective) {
        if (minimize_locks_)
          next_ev = acquire_event(next_event_id);
        clear_collective_deps(done_ev,next_ev);
        if (minimize_locks_)
          release_event(next_event_id);
      }
      if (!minimize_locks_)
        release_event(next_event_id);  
    } //else - no listener, possibly the last collective
  }

  void
  boxml::simple_event_done(int index)
  {
    event* done_ev = g_events[index];
    simple_event* ev = static_cast<simple_event*>(done_ev);
    int_container_t& listeners = ev->get_listeners();
    while (!listeners.empty()) {
      int frt = *(listeners.begin());

      event* evl;
      if (!minimize_locks_)
        evl = acquire_event(frt); 
      else
        evl = g_events[frt];

      switch (evl->event_type()){
        case event::computation:
        {
          comp_t* comp = static_cast<comp_t*>(evl);
          comp->remove_dep(index);
          if (debug_ > 1)
            printf("rank %d event %d to %d deps from %d\n", 
                   rank_, comp->index(), comp->n_dep(), ev->index());
          if (comp->n_dep() == 0) {
            g_rank_to_valid_comps[rank_].push_back(comp->index());
          }
          break;
        }
        case event::pt2pt:
        {
          comm_t* comm = static_cast<comm_t*>(evl);
          comm->remove_dep(index);
          if (debug_ > 1)
            printf("rank %d event %d to %d deps from %d\n", 
                   rank_, comm->index(), comm->n_dep(), ev->index());
          if (comm->n_dep() == 0)
            g_rank_to_valid_sends[rank_].push_back(comm->index());
          break;
        }
        case event::collective:
        {
          if (minimize_locks_)
            evl = acquire_event(frt);
          clear_collective_deps(ev, evl);
          if (minimize_locks_)
            release_event(frt);
          break;
        }
        case event::none:
          spkt_abort_printf("Invalid event: has none type");
      }
      listeners.erase(listeners.begin());
      if (!minimize_locks_)
        release_event(frt);
    }
  }

  void
  boxml::add_event_to_epoch(int rank, int epoch) {
    g_max_epoch_ = std::max(epoch,g_max_epoch_);
    if (g_rank_to_epoch_count.count(rank) != 0 &&
        g_rank_to_epoch_count[rank].count(epoch) != 0)
      ++g_rank_to_epoch_count[rank][epoch];
    else
      g_rank_to_epoch_count[rank][epoch] = 1;
  }

  void
  boxml::epoch_event_done(int epoch) {
    if (g_rank_to_epoch_done.count(rank_) != 0 &&
        g_rank_to_epoch_done[rank_].count(epoch) != 0)
      ++g_rank_to_epoch_done[rank_][epoch];
    else
      ++g_rank_to_epoch_done[rank_][epoch] = 1;
    if (debug_ > 2)
      printf("rank %d epoch %d counter incremented to %d of %d\n",
             rank_, epoch, g_rank_to_epoch_done[rank_][epoch],
             g_rank_to_epoch_count[rank_][epoch]);

    if (g_rank_to_epoch_count[rank_][epoch] ==
        g_rank_to_epoch_done[rank_][epoch]) {
      if (synch_mode_ == full_synch) {
        sstmac::timestamp start_bar = now();
        comm_barrier(barrier_tag_);
        comm_collective_block(sumi::collective::barrier, barrier_tag_);
        sstmac::timestamp end_bar = now();
        double bar_time = (end_bar - start_bar).sec();
        g_total_barrier_time += bar_time;
        ++barrier_tag_;
      }
      ++current_epoch_;
      if (debug_ > 2)
        printf("rank %d advanced current epoch to %d\n",
               rank_, current_epoch_);
    }
  }

  int 
  boxml::current_epoch() {
    if (synch_mode_ == full_asynch)
      return -1;
    test_for_valid_epoch();
    return current_epoch_;
  }

  void
  boxml::test_for_valid_epoch() {
    bool have_epoch_data = true;
    if (g_rank_to_epoch_count.count(rank_) == 0)
      have_epoch_data = false;
    if (have_epoch_data == true)
      if (g_rank_to_epoch_count[rank_].count(current_epoch_) == 0)
        have_epoch_data = false;
    if (have_epoch_data == false) {
      if (debug_ > 2)
        printf("rank %d no count found for epoch %d, advancing epoch\n", rank_, current_epoch_);
      if (synch_mode_ == full_synch) {
        comm_barrier(barrier_tag_);
        sstmac::timestamp start_bar = now();
        comm_collective_block(sumi::collective::barrier, barrier_tag_);
        //printf("rank %d finished full_synch block\n", rank_);
        sstmac::timestamp end_bar = now();
        double bar_time = (end_bar - start_bar).sec();
        g_total_barrier_time += bar_time;
        ++barrier_tag_;
      }
      ++current_epoch_;
      if (g_rank_to_epoch_count.count(rank_) == 1)
        test_for_valid_epoch();
    }
  }
  

} // end namespace lblxml