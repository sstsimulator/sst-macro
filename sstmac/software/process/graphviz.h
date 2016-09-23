#ifndef SSTMAC_SOFTWARE_PROCESS_GRAPHVIZ_H
#define SSTMAC_SOFTWARE_PROCESS_GRAPHVIZ_H

#define GraphVizAppendBacktrace(...) ::sstmac::sw::graph_viz_increment_stack __graphviz_tmp_variable__(__VA_ARGS__)
#define GraphVizDoNothing(...) int __graphviz_tmp_variable__

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/software/process/thread_fwd.h>

namespace sstmac {
namespace sw {


class graph_viz_increment_stack
{
 public:
  /**
   * @brief graph_viz_increment_stack
   *        Should only ever be called from app threads, not the DES thread
   * @param The name of the function currently being invoked
   * @param Optional boolean to turn off collection.
   *        There are certain cass where this might get called from
   *        the DES thread, which is an error. This allows
   *        the backtrace to be turned off on the DES thread
   */
  graph_viz_increment_stack(const char* fxn);

  ~graph_viz_increment_stack();

};

class graph_viz :
  public stat_collector
{
 public:
  graph_viz(sprockit::sim_parameters* params) :
    stat_collector(params)
  {
  }

  std::string
  to_string() const override {
    return "grahpviz";
  }

  virtual ~graph_viz();

  void simulation_finished(timestamp end) override;

  void clear() override;

  void reduce(stat_collector *coll) override;

  void dump_local_data() override;

  void dump_global_data() override;

  void
  global_reduce(parallel_runtime *rt) override;

  stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new graph_viz(params);
  }

  static void** allocate_trace();

  static void delete_trace(void** tr);

  void count_trace(long count, sw::thread* thr);

  static void
  delete_statics();

 private:
  typedef std::pair<long, long long> graphviz_call;
  class trace  {

   private:
    std::map<void*, graphviz_call> calls_;

    long long self_;

    void* fxn_;

    graph_viz* parent_;

   public:
    trace(graph_viz* parent, void* fxn);

    std::string
    summary() const;

    void* fxn() const;

    void add_call(void* fxn, int ncalls, long count);

    void add_self(long count);

  };

  void add_call(int ncalls, long count, void* fxn, void* callfxn);

  std::map<void*, trace*> traces_;

  std::map<void*, std::string> ptr_to_fxn_;

  std::map<std::string, void*> fxn_to_ptr_;

  friend class trace;

  trace* get_trace(void* fxn);


};

}
}


#endif // GRAPHVIZ_H

