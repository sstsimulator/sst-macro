#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>
#include <sstmac/common/thread_info.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/switch_interconnect.h>
#include <sprockit/util.h>
#include <sprockit/serializer.h>
#include <limits>

#define event_debug(...) \
  debug_printf(sprockit::dbg::parallel, "Rank %d: %s", rt_->me(), sprockit::printf(__VA_ARGS__).c_str())

RegisterDebugSlot(event_manager_time_vote);

#if SSTMAC_DEBUG_THREAD_EVENTS
DeclareDebugSlot(thread_events)
RegisterDebugSlot(thread_events)
#endif

namespace sstmac {
namespace native {

SpktRegister("clock_cycle_parallel", event_manager, clock_cycle_event_map,
    "Implements a parallel event queue with synchronization on regular clock cycles");

void
clock_cycle_event_map::init_factory_params(sprockit::sim_parameters* params)
{
  event_container::init_factory_params(params);
}

void
clock_cycle_event_map::finalize_init()
{
  epoch_ = 0;
  int64_t max_ticks = std::numeric_limits<int64_t>::max() - 100;
  no_events_left_time_ = timestamp(max_ticks, timestamp::exact);
  thread_incoming_.resize(nthread());
}

void
clock_cycle_event_map::schedule_incoming(const std::vector<void*>& buffers)
{
  sprockit::serializer ser;
  int num_bufs = buffers.size();
  int buf_size = rt_->ser_buf_size();
  for (int i=0; i < num_bufs; ++i){
    event_loc_id dst;
    event_loc_id src;
    uint32_t seqnum;
    timestamp time;
    sst_message* msg;
    void* buffer = buffers[i];
    ser.start_unpacking((char*)buffer, buf_size);
    ser & dst;
    ser & src;
    ser & seqnum;
    ser & time;
    ser & msg;
    event_handler* dst_handler;
    if (dst.is_switch_id()){
      switch_id sid = dst.convert_to_switch_id();
      event_debug("epoch %d: scheduling incoming event at %12.8e to switch %d",
        epoch_, time.sec(), int(sid));
      dst_handler = interconn_->switch_at(sid);
    } else {
      node_id nid = dst.convert_to_node_id();
      event_debug("epoch %d: scheduling incoming event at %12.8e to node %d",
        epoch_, time.sec(), int(nid));
      sstmac::hw::node* dst_node = interconn_->node_at(nid);
#if SSTMAC_SANITY_CHECK
      if (!dst_node){
        spkt_throw_printf(sprockit::value_error,
          "could not find node %d scheduling ipc message %s",
          int(msg->toaddr()), msg->to_string().c_str());
      }
#endif
      dst_handler = dst_node->get_nic();
    }
    schedule(time, seqnum, new handler_event(msg, dst_handler, src));
  }

  rt_->free_recv_buffers(buffers);
}

void
clock_cycle_event_map::receive_incoming_events()
{
#if SSTMAC_SANITY_CHECK
  if (thread_id() != 0){
    spkt_throw(sprockit::illformed_error,  
        "clock_cycle_event_map::schedule_incoming: only thread 0 should handle incoming MPI messages");
  }
#endif
  rt_->send_recv_messages(all_incoming_);

  if (nthread() == 1){
    schedule_incoming(all_incoming_);
  }
  else {
    int num_incoming = all_incoming_.size();
    for (int i=0; i < num_incoming; ++i){
      void* buffer = all_incoming_[i];
      int sid = *(reinterpret_cast<int*>(buffer));
      int thr = interconn_->thread_for_switch(switch_id(sid));
      thread_incoming_[thr].push_back(buffer);
    }
  }
  all_incoming_.clear();
}


int64_t
clock_cycle_event_map::do_vote(int64_t my_time, vote_type_t ty)
{
  switch (ty){
    case vote_max:
      return rt_->allreduce_max(my_time);
      break;
    case vote_min:
      return rt_->allreduce_min(my_time);
      break;
  }
}

timestamp
clock_cycle_event_map::vote_next_round(timestamp time, vote_type_t ty)
{
  int64_t vote_result = do_vote(time.ticks_int64(), ty);
  timestamp final_time(vote_result, timestamp::exact);
  event_debug("epoch %d got time %12.8e",
    epoch_, final_time.sec());
  return final_time;
}

bool
clock_cycle_event_map::vote_to_terminate()
{
  event_debug("epoch %d: voting to terminate on thread %d", 
    epoch_, thread_id());

  receive_incoming_events();
  timestamp my_vote = (empty() || stopped_) ? no_events_left_time_ : next_event_time();
  timestamp min_time = vote_next_round(my_vote, vote_min);
  ++epoch_;
  if (min_time == no_events_left_time_){
    return true; //done
  } else {
    next_time_horizon_ = min_time + lookahead_;
    return false;
  }
}

timestamp
clock_cycle_event_map::next_event_time() const
{
  return (*queue_.begin())->time();
}

#if DEBUG_DETERMINISM
std::map<event_loc_id,std::ofstream*> outs;
#endif

void
clock_cycle_event_map::do_next_event()
{
  timestamp ev_time = next_event_time();
  while (ev_time >= next_time_horizon_){
    receive_incoming_events();

    ev_time = next_event_time();

    event_debug("epoch %d: thread %d voting for min time %12.8e",
        epoch_, ev_time.sec());

    timestamp min_time = vote_next_round(ev_time, vote_min);
    next_time_horizon_ = min_time + lookahead_;

    event_debug("epoch %d: next time horizon is %12.8e for lookahead %12.8e: next event at %12.8e %sready to proceed on thread %d",
        epoch_, next_time_horizon_.sec(), lookahead_.sec(), ev_time.sec(),
        ((ev_time > next_time_horizon_) ? "not " : ""),
        thread_id());


    ++epoch_;
  }

  event* ev = pop_next_event();

#if DEBUG_DETERMINISM
  std::ofstream*& f = outs[ev->event_location()];
  if (f == 0){
    char fname[64];
    sprintf(fname, "events.%d.out", int(ev->event_location().location));
    f = new std::ofstream(fname);
  }

  *f << sprockit::printf("%ld: %d: %d<-%d", 
    ev->time().ticks_int64(), ev->seqnum(),
    int(ev->event_location().location), int(ev->src_location().location));
  *f << " " << ev->to_string() << std::endl;
#endif

  set_now(ev->time());
#if SSTMAC_DEBUG_THREAD_EVENTS
  if (sprockit::debug::slot_active(sprockit::dbg::thread_events)){
    event_file_ << sprockit::printf("T=%10.5fms: %s\n",
                    ev->time().msec(), ev->to_string().c_str());
  }
#endif
  ev->execute();
  delete ev;
}

void
clock_cycle_event_map::run()
{
  event_map::run();
  timestamp global_max = vote_next_round(now(), vote_max);
  set_now(global_max);
}

#if SSTMAC_DEBUG_THREAD_EVENTS
void
clock_cycle_event_map::open_debug_file()
{
  if (sprockit::debug::slot_active(sprockit::dbg::thread_events)){
    std::string fname = sprockit::printf("events.thread%d", thread_id());
    event_file_.open(fname.c_str());
  }
}

void
clock_cycle_event_map::close_debug_file()
{
  if (sprockit::debug::slot_active(sprockit::dbg::thread_events)){
    event_file_.close();
  }
}
#endif

void
clock_cycle_event_map::set_interconnect(hw::interconnect* interconn)
{
  event_map::set_interconnect(interconn);
  int nworkers = rt_->nproc() * rt_->nthread();
  if (nworkers == 1){
    //dont need the interconnect
    lookahead_ = timestamp(1e5);
  }
  else {
    interconn_ = safe_cast(hw::switch_interconnect, interconn,
                "parallel DES only compatible with switch interconnect");
    lookahead_ = interconn_->lookahead();
  }
  next_time_horizon_ = lookahead_;
}

void
clock_cycle_event_map::ipc_schedule(timestamp t,
  event_loc_id dst,
  event_loc_id src,
  uint32_t seqnum,
  sst_message* msg)
{
  event_debug("epoch %d: scheduling outgoing event at t=%12.8e to location %d",
    epoch_, t.sec(), int(dst.convert_to_switch_id()));

  rt_->send_message(thread_id_, t,
    dst.convert_to_switch_id(),
    src,
    seqnum,
    msg);
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
