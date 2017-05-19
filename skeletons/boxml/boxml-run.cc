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
#include <algorithm>
#include <ctime>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/stats/stat_local_double.h>

using namespace std;
using namespace sstmac;


namespace lblxml
{
  int boxml::count_events () {
    int ret = g_rank_to_comps[rank_].size()
        + g_rank_to_sends[rank_].size()
        + g_rank_to_recvs[rank_].size()
        + g_rank_to_allreduces[rank_].size();
    return ret;
  }

  bool boxml::send_boxes(int& n_events) {
      SSTMACBacktrace("send boxes");
      // any sends ready?
      da_list_t& v_sends(g_rank_to_valid_sends[rank_]);
      set_t& my_sends(g_rank_to_sends[rank_]);
      int n_send = v_sends.size();
      if (randomize_events_)
        std::random_shuffle(v_sends.begin(), v_sends.end());
      int n_sent = 0;
      for (da_list_t::iterator it = v_sends.begin();
           it != v_sends.end(); ) {
        comm_t* sendptr = static_cast<comm_t*>(g_events[*it]);
        comm_t& send = *sendptr;
        int count = send.size();
        int dest_box = send.to();
        int dest = g_boxindex_to_rank[ dest_box ];
        int index = send.index();
        int epoch = send.epoch();
        if (debug_ > 1)
          printf("rank %d considering send %d to %d\n", rank_, index, dest);
        if( dest != rank_ ) {
          if (synch_mode_ >= phase_asynch || epoch == current_epoch() ) {
            ++n_sent;
            if (debug_ > 0)
              printf("rank %d sending message %d to rank %d\n",
                     rank_, index, dest);
            g_message_begin_[index] = now();
            pt2pt_message::ptr mess = new pt2pt_message(index,count);
            comm_rdma_put(dest, mess);
            if (synch_mode_ < phase_asynch) {
              if (debug_ > 0 && rank_ == BOXML_DEBUG_RANK && epoch == BOXML_DEBUG_EPOCH )
                printf("rank %d completed event %d for epoch %d\n", BOXML_DEBUG_RANK, index, BOXML_DEBUG_EPOCH);
              epoch_event_done(epoch);
            }
            my_sends.erase(send.index());
            it = v_sends.erase(it);
          }
          else {
            if (debug_ > 1)
              printf("rank %d did not perform send %d with epoch %d, current epoch %d\n",
                     rank_, index, epoch, current_epoch());
            ++it;
          }
        }
        else {
          // local send, noop, handle its completion
          // note that n_events doesn't include sends local to rank
          simple_event_done(index);
          my_sends.erase(send.index());
          it = v_sends.erase(it);
        }
      }

      n_events -= n_sent;
      if (n_sent > 0)
        return true;
      return false;
  }

  /**
   * @brief boxml::reduce
   * @param n_events [IN,OUT]  Number of outstanding events - decrement
   * @return
   */
  bool boxml::reduce(int& n_events)
  {
    SSTMACBacktrace("reduce");
    allreduce_queue_t temp_queue;
    int n_allreduce = 0;
    while (!valid_allreduces_.empty()) {
      index_box_pair_t pair = valid_allreduces_.front();
      int index = pair.first;
      int box_number = pair.second;
      event* ev = g_events[index];
      if (synch_mode_ >= phase_asynch || ev->epoch() == current_epoch()) {
        reduce_t* comm = static_cast<reduce_t*>(ev);
        int my_domain_rank = comm->domain_rank(box_number);
        const int* boxes = comm->box_array();
        sumi::communicator* dom =
            new box_domain(my_domain_rank, comm->nboxes(), boxes,
                           g_boxindex_to_rank.data());

        int count = comm->size();
        if (debug_ > 0)
          printf("rank %d starting allreduce %s for box %d\n",
                 rank_, ev->id().c_str(), box_number);
        sumi::comm_allreduce<double,sumi::Add>(NULL, NULL, count, index,
                                               false,
                                               sumi::options::initial_context,
                                               dom);
        valid_allreduces_.pop();
        ++n_allreduce;
      }
      else {
        temp_queue.push(valid_allreduces_.front());
        valid_allreduces_.pop();
      }
    }

    valid_allreduces_.swap(temp_queue);

    if (n_allreduce > 0)
      return true;
    return false;
  }

  bool boxml::compute_boxes(int& n_events) {

    SSTMACBacktrace("compute boxes");
    da_list_t& v_comps(g_rank_to_valid_comps[rank_]);
    set_t& my_comps(g_rank_to_comps[rank_]);
    int n_comp = 0;

    if (randomize_events_)
      std::random_shuffle(v_comps.begin(), v_comps.end());

    for( da_list_t::iterator comp_it = v_comps.begin();
         comp_it != v_comps.end(); ++comp_it) {
      if (debug_ > 0)
        printf("rank %d considering compute %d for execution\n", rank_, *comp_it);
      comp_t* compptr = static_cast<comp_t*>(g_events[*comp_it]);
      comp_t& comp = *compptr;
      int ev_epoch = comp.epoch();
      if (!ignore_epoch_ && ev_epoch < current_epoch()) {
        std::cerr << "trying to run event from earlier epoch\n";
        abort();
      }
      if (synch_mode_ == full_asynch || ev_epoch == current_epoch()) {
        if (do_compute_) {
          sstmac::timestamp start_comp = now();
          compute( timestamp( compute_scale_ * comp.time() ));
          sstmac::timestamp end_comp = now();
          double comp_time = (end_comp - start_comp).sec();
          g_total_compute_time += comp_time;
        }
        if (debug_ > 0)
          printf("rank %d: all deps satisfied, performing compute %s\n",
                 rank_, comp.id().c_str());
        my_comps.erase(comp.index());
        v_comps.erase(comp_it);
        if (synch_mode_ < full_asynch) {
          if (debug_ > 0 && rank_ == BOXML_DEBUG_RANK && ev_epoch == BOXML_DEBUG_EPOCH )
            printf("rank %d completed event %d for epoch %d\n", BOXML_DEBUG_RANK, comp.index(), BOXML_DEBUG_EPOCH);
          epoch_event_done(ev_epoch);
        }
        simple_event_done(comp.index());
        n_events -= 1; // only do one compute at a time
        n_comp = 1;
        break;
      }
      else
        if (debug_ > 0)
          printf("rank %d not running compute event %d event/current epochs %d/%d\n", rank_, *comp_it, ev_epoch, current_epoch());
    }

    if (n_comp > 0)
      return true;
    return false;
  }

  void boxml::recv_boxes(int& n_events)
  {
      SSTMACBacktrace("recv boxes");
      sstmac::timestamp start_poll = now();
      if (debug_ > 0)
        printf("rank %d polling for new message\n",rank_);
      sumi::message::ptr dmess = sumi::comm_poll();
      sstmac::timestamp end_poll = now();
      double poll_time = (end_poll - start_poll).sec();
      g_total_idle_time += poll_time;
      if (idle_time_) {
        idle_time_->collect(poll_time);
      }
      int epoch;
      int index;
      switch (dmess->class_type()){
        case sumi::message::pt2pt:
        {
          pt2pt_message::ptr pmess = ptr_safe_cast(pt2pt_message, dmess);
          index = pmess->event_index();
          event* ev = g_events[index];
          epoch = ev->epoch();
          if (debug_ > 0)
            printf("rank %d receiving pt2pt message %d\n", rank_, index);
          if (hist_eff_bw_) {
            comm_t* sendptr = static_cast<comm_t*>(g_events[index]);
            comm_t& send = *sendptr;
            int size = send.size();
            double time = (now() - g_message_begin_[index]).sec();
            double bw = double(size) / time;
            hist_eff_bw_->collect(bw);
          }
          simple_event_done(index);
          break;
        }
        case sumi::message::collective_done:
        {
          sumi::collective_done_message::ptr cmess = ptr_safe_cast(sumi::collective_done_message, dmess);
          box_domain* dom = static_cast<box_domain*>(cmess->dom());
          int my_box_number = dom->my_box_number();
          index = cmess->tag();
          event* ev = g_events[index];
          epoch = ev->epoch();
          if (debug_ > 0)
            printf("rank %d receiving collective_done message for %d\n",
                   rank_, cmess->tag() );
          collective_done(my_box_number, cmess->tag());
          //for now, we dynamically create domains - delete it
          delete dom;
          break;
        }
        default:
        {
          spkt_throw_printf(sprockit::value_error,
            "got invalid message type %s",
            sumi::message::tostr(dmess->class_type()));
        }
      }

      --n_events;
      if (synch_mode_ < phase_asynch) {
        epoch_event_done(epoch);
        if (debug_ > 0 && rank_ == BOXML_DEBUG_RANK && epoch == BOXML_DEBUG_EPOCH )
          printf("rank %d completed event %d for epoch %d\n", BOXML_DEBUG_RANK, index, BOXML_DEBUG_EPOCH);
      }
  }

  void boxml::run_loop() {
    SSTMACBacktrace("run loop"); 

    if (debug_ > 0)
      printf("rank %d entering run loop with %d recvs\n",
             rank_, (int)g_rank_to_recvs[rank_].size());

    // Loop until no events left
    int total_events = count_events();
    //printf("rank %d total_events = %d\n", rank_, total_events);
    int n_events = total_events;
    int q_events = total_events/4;
    bool q1 = false, q2 = false, q3 = false;
    if (n_events > 0) ++g_active_ranks;
    else {
      //printf("rank %d g_max_epoch_=%d\n", rank_, g_max_epoch_);
      // needed to trigger full_synch barriers
      if (synch_mode_ == full_synch)
        while (current_epoch() < g_max_epoch_) {
          //printf("rank %d current_epoch %d, g_max_epoch_ %d\n", rank_, current_epoch(), g_max_epoch_);
      }
    }
    
    while (n_events > 0) {

      if (debug_ > 0 || detailed_progress_ ) {
        if (n_events < 3 * q_events && q1 == false) {
          cout << "Rank " << rank_ << " 25% complete\n";
          q1 = true;
        }
        if (n_events < 2 * q_events && q2 == false) {
          cout << "Rank " << rank_ << " 50% complete\n";
          q2 = true;
        }
        if (n_events < q_events && q3 == false) {
          cout << "Rank " << rank_ << " 75% complete\n";
          q3 = true;
        }
      }
      else if (rank_ == 0) {
        if (n_events < 3 * q_events && q1 == false) {
          cout << "Rank 0 25% complete\n";
          q1 = true;
        }
        if (n_events < 2 * q_events && q2 == false) {
          cout << "Rank 0 50% complete\n";
          q2 = true;
        }
        if (n_events < q_events && q3 == false) {
          cout << "Rank 0 75% complete\n";
          q3 = true;
        }
      }

      if(detailed_progress_ && rank_ == 0) {
        cout << "Rank 0 completed " << total_events - n_events << " of " << total_events
             << " events\n";
        cout << std::flush;
        cerr << std::flush;
      }

      bool skip_to_next_loop = send_boxes(n_events);
      if (skip_to_next_loop)
        continue;

      skip_to_next_loop = compute_boxes(n_events);
      if (skip_to_next_loop)
        continue;

      skip_to_next_loop = reduce(n_events);
      if (skip_to_next_loop)
        continue;

      recv_boxes(n_events);
    }

    if (debug_ > 0)
      printf("rank %d current_epoch is %d\n", rank_, current_epoch());

    if (debug_ > 0)
      printf("Rank %d completed run loop\n", rank_);

  }

}