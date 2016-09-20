#ifndef LOCATION_TRACE_H
#define LOCATION_TRACE_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/event_location.h>

namespace sstmac {

class location_trace :
  public stat_collector
{

 public:
  location_trace(sprockit::sim_parameters* params) :
    stat_collector(params)
  {
  }

  std::string
  to_string() const override {
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
  dump_local_data() override;

  void
  dump_global_data() override;

  void
  global_reduce(parallel_runtime *rt) override;

  void
  simulation_finished(timestamp end) override {}

  location_trace*
  clone_me(int id) const {
    location_trace* cln = new location_trace(params_);
    cln->set_id(id);
    return cln;
  }

  stat_collector*
  clone() const override {
    return clone_me(-1);
  }

  void
  reduce(stat_collector* coll) override;

  void clear() override;

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

