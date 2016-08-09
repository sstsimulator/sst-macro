#ifndef sstmac_software_process_FTQ_H
#define sstmac_software_process_FTQ_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/key.h>
#include <stdint.h>
#include <vector>

#include <sprockit/unordered.h>

#include <string>
#include <stdlib.h>

/**
  "Fixed time quanta" collection of the amount of work done in particular time intervals
  over the course of the computation
*/
namespace sstmac {
namespace sw {


class ftq_epoch
{
 protected:
  long long* totals_;

 public:
  ftq_epoch();

  virtual ~ftq_epoch();

  void
  collect(int key_typeid, long ticks) {
    totals_[key_typeid] += ticks;
  }

  void init(int num_events, long long* buffer);

  long long
  event_time(int key_typeid) const {
    return totals_[key_typeid];
  }

  void
  set_event_time(int key_typeid, long long count) {
    totals_[key_typeid] = count;
  }

};

struct event_node {
  int event_typeid;

  long ticks_begin;

  long ticks_end;

  event_node* next;

};

class task_ftq_calendar :
  public ftq_epoch
{
 public:

  task_ftq_calendar();

  virtual ~task_ftq_calendar();

  void collect(int event_typeid, long ticks_begin, long ticks);

  virtual std::string
  to_string() const {
    return "TaskFTQCalendar";
  }

  void dump(std::ofstream& os);

 protected:
  event_node* head_;

  event_node* tail_;

  long max_tick_;


};

class app_ftq_calendar :
  public task_ftq_calendar
{

 public:
  app_ftq_calendar(int aid,
                   const std::string& appname,
                   long nticks_epoch);

  void dump(const std::string& fileroot);

  virtual ~app_ftq_calendar();

  /**
    Resolution of the ticks is set by timestamp_resolution parameter.
    timestamp_resolution gives the number of ps per tick
    timestamp_resolution=100 -> 100ps = 1 tick
    @param event_typeid The type of event (MPI,Compute,Sleep,etc)
    @param tid The task id (essentially MPI Rank)
    @param ticks_begin The time the event started
    @param num_ticks The duration of the event
  */
  void collect(int event_typeid, int tid, long ticks_begin, long num_ticks);

  virtual std::string
  to_string() const {
    return "AppFTQCalendar";
  }

  void
  reduce(app_ftq_calendar* cal);

  void
  global_reduce(parallel_runtime* rt);

 private:
  spkt_unordered_map<int, task_ftq_calendar*> calendars_;
  spkt_unordered_map<int, ftq_epoch*> thread_epochs_;

  std::vector<ftq_epoch> epochs_;

  std::list<long long*> buffers_;

  int aid_;

  int max_tid_;

  long max_epoch_;

  long max_epoch_allocated_;

  long num_ticks_epoch_;

  std::string appname_;

  void dumpi_gnuplot_histogram(const std::string& fileroot, int num_categories);

  void allocate_epochs(long max_epoch);

  static const long allocation_num_epochs;

};

class ftq_calendar :
  public stat_collector
{
 public:
  ftq_calendar();

  void init(long nticks_per_epoch);

  virtual ~ftq_calendar();

  void collect(int event_typeid,
               int aid,
               int tid,
               long ticks_begin,
               long num_ticks);

  app_ftq_calendar* get_calendar(int aid) const;

  void register_app(int aid, const std::string& appname);

  void simulation_finished(timestamp end);

  void dump_local_data();

  void dump_global_data();

  void clear();

  void reduce(stat_collector* coll);

  void global_reduce(parallel_runtime *rt);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  ftq_calendar*
  clone_me(int id) const {
    ftq_calendar* cln = new ftq_calendar;
    clone_into(cln);
    cln->set_id(id);
    return cln;
  }

  stat_collector*
  clone() const {
    return clone_me(-1);
  }

  virtual std::string
  to_string() const {
    return "FTQCalendar";
  }

 protected:
  void
  clone_into(ftq_calendar* cln) const;

 private:
  static spkt_unordered_map<int, app_ftq_calendar*> calendars_;

  long num_ticks_epoch_;



};

}
}

#endif // FTQ_H

