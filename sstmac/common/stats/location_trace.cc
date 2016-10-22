#include <sstmac/common/stats/location_trace.h>
#include <sprockit/util.h>

namespace sstmac {

SpktRegister("location_trace", stat_collector, location_trace);

#define cast_bytes(x) \
    reinterpret_cast<char*>(&x)

void
location_trace::collect(
  timestamp created,
  device_id creator,
  timestamp scheduled,
  device_id runner)
{
  event ev;
  ev.created = created;
  ev.creator = creator;
  ev.scheduled = scheduled;
  ev.runner = runner;
  local_events_.push_back(ev);
}

void
location_trace::clear()
{
  local_events_.clear();
}

void
location_trace::reduce(stat_collector *coll)
{
  location_trace* tr = safe_cast(location_trace, coll);
  std::list<event>::iterator it, end = tr->local_events_.end();
  for (it=tr->local_events_.begin(); it != end; ++it){
    event& ev = *it;
    global_events_[ev.scheduled] = ev;
  }
}

void
location_trace::global_reduce(parallel_runtime *rt)
{
  spkt_throw(sprockit::unimplemented_error,
    "location_trace::global_reduce: location trace should not be run in parallel");
}

void
location_trace::dump_global_data()
{
  std::string fname = sprockit::printf("%s.events.bin", fileroot_.c_str());
  std::fstream myfile;
  stat_collector::check_open(myfile, fname, std::ios::out | std::ios::binary);

  std::map<timestamp, event>::iterator it, end = global_events_.end();
  for (it=global_events_.begin(); it != end; ++it){
    event& ev = it->second;
    myfile.write(cast_bytes(ev), sizeof(event));
  }
  myfile.close();
}

void
location_trace::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d.events.bin", fileroot_.c_str(), id_);
  std::fstream myfile;
  stat_collector::check_open(myfile, fname, std::ios::out | std::ios::binary);

  std::list<event>::iterator it, end = local_events_.end();
  for (it=local_events_.begin(); it != end; ++it){
    event& ev = *it;
    myfile.write(cast_bytes(ev), sizeof(event));
  }
  myfile.close();
}

bool
location_trace::read(
  std::istream& myfile,
  timestamp &created,
  device_id &creator,
  timestamp &scheduled,
  device_id &runner)
{
  if (myfile.eof()) {
    return false;
  }

  event ev;
  myfile.read(cast_bytes(ev), sizeof(event));
  created = ev.created;
  creator = ev.creator;
  scheduled = ev.scheduled;
  runner = ev.runner;
  return true;
}

}

