#ifndef LOCATION_TRACE_H
#define LOCATION_TRACE_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/event_location.h>

namespace sstmac {

class location_trace :
  public stat_collector
{

 public:
  std::string
  to_string() const {
    return "location trace";
  }

  void
  collect(timestamp created,
          event_loc_id creator,
          timestamp scheduled,
          event_loc_id runner);

  bool
  read(std::istream& myfile,
       timestamp& created,
       event_loc_id& creator,
       timestamp& scheduled,
       event_loc_id& runner);

  void
  dump_local_data();

  void
  dump_global_data();

  void
  global_reduce(parallel_runtime *rt);

  void
  simulation_finished(timestamp end){}

  location_trace*
  clone_me(int id) const {
    location_trace* cln = new location_trace;
    cln->set_id(id);
    return cln;
  }

  stat_collector*
  clone() const {
    return clone_me(-1);
  }

  void
  reduce(stat_collector* coll);

  void clear();

  virtual ~location_trace() {}


 private:
  struct event {
    timestamp created;
    event_loc_id creator;
    timestamp scheduled;
    event_loc_id runner;
  };

  std::list<event> local_events_;

  std::map<timestamp, event> global_events_;

};

}

#endif // LOCATION_TRACE_H

