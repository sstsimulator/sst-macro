#include "boxml.h"
#include <algorithm>
#include <ctime>
#include <sst/sumi_api.h>
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
      while (!v_sends.empty()) {
        comm_t* sendptr = static_cast<comm_t*>(g_events[v_sends.front()]);
        comm_t& send = *sendptr;
        if (send.n_dep() == 0) { // abundance of caution
          int count = send.size();
          int dest_box = send.to();
          int dest = g_boxindex_to_rank[ dest_box ];
          int index = send.index();
          if( dest != rank_ ) {
            ++n_sent;
            if (debug_ > 0)
              std::cerr << "boxml: rank " << rank_ << " sending message " << index << " to rank " << dest <<  "\n";
            g_message_begin_[index] = now();
            pt2pt_message::ptr mess = new pt2pt_message(index,count);
            comm_rdma_put(dest, mess);
          }
          else {
            simple_event_done(index);
          }
          my_sends.erase(send.index());
          v_sends.pop_front();
        }
        else {
          std::cerr << "error: valid send has n_deps > 0\n";
          abort();
        }
      }

      int old_n_events = n_events;
      n_events -= n_sent;
      //events aren't tracked properly anymore
      if (0) {
        cout << "Rank " << rank_ << " posted " << n_sent << " sends\n";
        if (n_send > 0) {
          int temp_n = count_events();
          if (temp_n != n_events) {
            std::cerr << "send_boxes: n_events " << n_events << " doesn't match count_events() "
                         << temp_n << "\n";
            abort();
          }
          cout << "Rank " << rank_ << " after send processing, "
               << " continuing while loop with " << n_events
               << " events remaining (delta=" << n_events - old_n_events
               << ")\n";
        }
      }

      if (n_sent > 0)
        return true;
      return false;
  }

  /**
   * @brief boxml::reduce
   * @param n_events [IN,OUT]  Number of outstanding events - decrement
   * @return
   */
  bool boxml::reduce(int& n_events) {
    SSTMACBacktrace("reduce");
    int n_allreduce = valid_allreduces_.size();
    while (!valid_allreduces_.empty()) {
      index_box_pair_t pair = valid_allreduces_.front();
      valid_allreduces_.pop();
      int index = pair.first;
      int box_number = pair.second;
      event* ev = g_events[index];
      reduce_t* comm = static_cast<reduce_t*>(ev);
      int my_domain_rank = comm->domain_rank(box_number);
      const int* boxes = comm->box_array();
      sumi::domain* dom = new box_domain(my_domain_rank, comm->nboxes(), boxes, g_boxindex_to_rank.data());

      int count = comm->size();
      if (debug_ > 0)
        std::cerr << "boxml: rank " << rank_ << " starting allreduce " << ev->id() << " for box " << box_number << "\n";
      sumi::comm_allreduce<double,sumi::Add>(NULL, NULL, count, index,
                                 false, sumi::options::initial_context, dom);
    }
    return n_allreduce > 0;
  }

  bool boxml::compute_boxes(int& n_events) {
      SSTMACBacktrace("compute boxes");
      da_list_t& v_comps(g_rank_to_valid_comps[rank_]);
      set_t& my_comps(g_rank_to_comps[rank_]);
      int n_comp = v_comps.size();

      if (n_comp) {
        if (randomize_events_)
          std::random_shuffle(v_comps.begin(), v_comps.end());

        comp_t* compptr = static_cast<comp_t*>(g_events[v_comps.front()]);
        comp_t& comp = *compptr;
        if (comp.n_dep() == 0) { // Abundance of caution
          if (do_compute_) {
            compute( timestamp( compute_scale_ * comp.time() ));
          }
          if (debug_ > 0) {
            cout << "boxml: rank " << rank_ << ": all deps satisfied, performing compute "
                 << comp.id() << " on " << rank_ << "\n";
          }
          my_comps.erase(comp.index());
          v_comps.pop_front();
          simple_event_done(comp.index());
          n_events -= 1; //I only do one compute at a time
        }
        else {
          spkt_throw_printf(sprockit::value_error,
            "valid compute event %d still has %d deps",
            comp.index(), comp.n_dep());
        }
      }

      n_comp = min(n_comp,1);
      int old_n_events = n_events;
      // events aren't properly tracked anymore
      if (0) {
        cout << "Rank " << rank_ << " completed " << n_comp <<  " computes\n";
        if (n_comp) {
          int temp_n = count_events();
          if (temp_n != n_events) {
            std::cerr << "compute_boxes: n_events doesn't match count_events()\n";
            abort();
          }
          cout << "Rank " << rank_ << " after compute processing, "
               << " continuing while loop with " << n_events
               << " events remaining (delta=" << n_events - old_n_events
               << ")\n";
        }
      }
      if (n_comp > 0)
        return true;
      return false;
  }

  void boxml::recv_boxes(int& n_events)
  {
      SSTMACBacktrace("recv boxes");
      sstmac::timestamp start_poll = now();
      sumi::message::ptr dmess = sumi::comm_poll();
      sstmac::timestamp end_poll = now();
      double poll_time = (end_poll - start_poll).sec();
      g_total_idle_time += poll_time;
      if (idle_time_) {
        idle_time_->collect(poll_time);
      }
      switch (dmess->class_type()){
        case sumi::message::pt2pt:
        {
          if (debug_ > 0)
            std::cerr << "boxml: rank " << rank_ << " receiving pt2pt message\n";
          pt2pt_message::ptr pmess = ptr_safe_cast(pt2pt_message, dmess);
          int index = pmess->event_index();
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
          if (debug_ > 0)
            std::cerr << "boxml: rank " << rank_ << " receiving collective_done message for " << cmess->tag() << "\n";
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

      int old_n_events = n_events;
      --n_events;
      // events aren't properly tracked anymore
      if (0) {
        cout << "Rank " << rank_ << " handled receive\n";
        int temp_n = count_events();
        if (temp_n != n_events) {
          std::cerr << "recv_boxes: n_events " << n_events << " doesn't match count_events() "
                    << temp_n << "\n";
          abort();
        }
        cout << "Rank " << rank_ << " after recv processing, "
             << " continuing while loop with " << n_events
             << " events remaining (delta=" << n_events - old_n_events
             << ")\n";
      }
  }

  void boxml::run_loop() {
    SSTMACBacktrace("run loop"); 

    //std::cerr << "Rank 0 entering run loop with "
    //          << g_rank_to_recvs[0].size() << " recvs\n";
    if (debug_ > 0) cout << "Rank " << rank_ << " entering run loop with "
                         << g_rank_to_recvs[rank_].size() << " recvs\n";

    // Loop until no elements left
    int total_events = count_events();
    int n_events = total_events;
    int q_events = total_events/4;
    bool q1 = false, q2 = false, q3 = false;
    if (n_events > 0) ++g_active_ranks;
    while (n_events > 0) {
    bool doloop = true;
    //while (doloop) {

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

      //std::cerr << "Rank before send boxes with "
      //          << g_rank_to_recvs[0].size() << " recvs\n";

      bool skip_to_next_loop = send_boxes(n_events);
      if (skip_to_next_loop)
        continue;

      //std::cerr << "Rank before compute boxes with "
      //          << g_rank_to_recvs[0].size() << " recvs\n";

      skip_to_next_loop = compute_boxes(n_events);
      if (skip_to_next_loop)
        continue;

      //std::cerr << "Rank before reduce with "
      //          << g_rank_to_recvs[0].size() << " recvs\n";

      skip_to_next_loop = reduce(n_events);
      if (skip_to_next_loop)
        continue;

      //std::cerr << "Rank before recv with "
      //          << g_rank_to_recvs[0].size() << " recvs\n";

      recv_boxes(n_events);

      //if( n_events == count_events())
      //  sstmac_usleep(1000);

      doloop = false;
    }

    if (debug_ > 0)
      cout << "Rank " << rank_ << " completed run loop\n";

  }

}
