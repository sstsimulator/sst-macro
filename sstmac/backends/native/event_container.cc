
#include <sstmac/common/sstmac_config.h>

#include <sstmac/backends/native/event_container.h>
#include <sprockit/sim_parameters.h>
#include <ctime>
#include <cmath>
#include <fstream>

DeclareDebugSlot(all_events)
RegisterDebugSlot(all_events)

namespace sstmac {
namespace native {

static uint64_t num_events = 0;

//
// Hello.
//
event_container::event_container() :
  running_(false),
  event_rate_reporting_(false),
  event_rate_window_(0)
{
  set_now(timestamp(0));
}


void
event_container::do_next_event()
{
  event_queue_entry* ev = pop_next_event();
  set_now(ev->time());
  debug_printf(sprockit::dbg::all_events,
    "running event %s", ev->to_string().c_str());


  ev->execute();
  delete ev;
}

void
event_container::init_factory_params(sprockit::sim_parameters* params)
{
  /**
    sstkeyword {
      docstring=Enables event rate reporting.ENDL
      If set to true, the number of events completed per time window will be printed.
      This can only be enabled if sanity check is enabled by configure.;
    }
  */
  event_rate_reporting_ =
      params->get_optional_bool_param("sanity_check_event_rate_reporting",false);
  /**
    sstkeyword {
      docstring=Sets the time window for event rate reporting (in seconds).ENDL
      The default is 60 seconds.;
    }
  */
  event_rate_window_ =
      params->get_optional_int_param("sanity_check_event_rate_window", 60);
  event_manager::init_factory_params(params);
} 


#if DEBUG_DETERMINISM
extern std::map<event_loc_id,std::ofstream*> outs;
#endif
 
//
// Run the eventmanager.
//
void
event_container::run()
{
  if (running_) {
    spkt_throw(sprockit::illformed_error,
              "event_map::run: event manager already running.");
  }
  running_ = true;
  stopped_ = false;

#if SSTMAC_SANITY_CHECK
  int n_events = 0;
  clock_t t1 = clock();
  clock_t t2;
#endif

#if SSTMAC_DEBUG_THREAD_EVENTS
  open_debug_file();
#endif

  while (1){
    while (!empty() && !stopped_) {
      do_next_event();

#if SSTMAC_SANITY_CHECK
      if(event_rate_reporting_) {
        ++n_events;
        t2 = clock() - t1;
        if (t2/CLOCKS_PER_SEC > event_rate_window_) {
          std::cout << n_events << " events executed in last "
                    << event_rate_window_ << "s\n";
          t1 = clock();
          n_events = 0;
        }
      }
#endif
    }
    bool terminate = vote_to_terminate();
    if (terminate)
      break;
  }

#if SSTMAC_DEBUG_THREAD_EVENTS
  close_debug_file();
#endif

  running_ = false;

  if (empty() || finish_on_stop_) {
    complete_ = true;
    finish();
  }

#if DEBUG_DETERMINISM
  std::map<event_loc_id,std::ofstream*>::iterator it, end = outs.end();
  for (it=outs.begin(); it != end; ++it){
    it->second->close();
  }
#endif
}


void
event_container::schedule(timestamp start_time, uint32_t seqnum, event_queue_entry* ev)
{
  if (start_time < now()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "event_map::schedule: scheduling event in the past: now=%ld units, ev=%ld units",
                     now().ticks(), start_time.ticks());
  }

  ev->set_time(start_time);
  ev->set_seqnum(seqnum);

  double delta = fabs(start_time.sec() - 1.29380e-04);
  if (delta < 1e-5){
    fflush(stdout);
    //abort();
  }

  debug_printf(sprockit::dbg::all_events,
    "adding event to run at %10.5e: %s",
    start_time.sec(), ev->to_string().c_str());
  add_event(ev);
}

void
event_container::finish()
{
}

}
}

