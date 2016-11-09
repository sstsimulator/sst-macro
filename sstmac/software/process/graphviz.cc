#include <sstream>

#include <sstmac/software/process/graphviz.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/thread_lock.h>

#if SSTMAC_HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#if SSTMAC_HAVE_DLFCN_H
#include <dlfcn.h>
#endif

namespace sstmac {
namespace sw {

#define BACKTRACE_NFXN 50
typedef void* graphviz_trace[BACKTRACE_NFXN];

static sprockit::need_delete_statics<graph_viz> del_statics;

SpktRegister("graph_viz | call_graph", stat_collector, graph_viz);

graph_viz_increment_stack::graph_viz_increment_stack(const char *fxn) 
{
  thread* thr = operating_system::current_thread();
  if (thr) {
    thr->append_backtrace(const_cast<char*>(fxn));
  } else {
   spkt_abort_printf("graphviz: operating system has no current thread");
  }
}

graph_viz_increment_stack::~graph_viz_increment_stack()
{
  thread* thr = operating_system::current_thread();
  if (thr) {
    thr->pop_backtrace();
  }
}

void
graph_viz::trace::add_call(void* fxn, int ncalls, long count)
{
  static thread_lock lock;
  lock.lock();
  graphviz_call& call = calls_[fxn];
  call.first += ncalls;
  call.second += count;
  lock.unlock();
}

void
graph_viz::trace::add_self(long count)
{
  self_ += count;
}

void
graph_viz::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() > 1){
    spkt_throw(sprockit::unimplemented_error, 
      "graph_viz::global_reduce: graphviz not available in parallel");
  }
}

std::string
graph_viz::trace::summary() const
{
  std::stringstream sstr;
  sstr << "fn=" << (const char*) fxn_ << "\n";
  sstr << 0 << " " << self_ << "\n";
  for (auto& pair : calls_) {
    const char* fxn = (const char*) pair.first;
    const graphviz_call& call = pair.second;
    sstr << "cfn=" << fxn << "\n";
    sstr << "calls=" << call.first << " " << 0 << "\n";
    sstr << 0 << " " << call.second << "\n";
  }
  return sstr.str();
}

void*
graph_viz::trace::fxn() const
{
  return fxn_;
}

graph_viz::trace::trace(graph_viz* parent, void* fxn)
  : self_(0), fxn_(fxn), parent_(parent)
{

}

graph_viz::trace*
graph_viz::get_trace(void* fxn)
{
  return traces_[fxn];
}

void
graph_viz::dump_local_data()
{
  spkt_throw(sprockit::unimplemented_error, "graph_viz::dump_local_data");
}

void
graph_viz::dump_global_data()
{
  std::fstream myfile;
  std::string fname = sprockit::printf("%s.callgrind.out", fileroot_.c_str());
  check_open(myfile, fname.c_str());

  myfile << "events: Instructions\n\n";

  for (auto& pair : traces_) {
    myfile << pair.second->summary();
    myfile << "\n";
  }
  myfile.close();
}

void
graph_viz::simulation_finished(timestamp t)
{
}

void
graph_viz::add_call(
  int ncalls,
  long count,
  void* fxn,
  void* callfxn
)
{
  trace* tr = traces_[fxn];
  if (!tr) {
    tr = new trace(this, fxn);
    traces_[fxn] = tr;
  }
  tr->add_call(callfxn, ncalls, count);
}

void
graph_viz::delete_statics()
{
}

graph_viz::~graph_viz()
{
  sprockit::delete_vals(traces_);
}

void**
graph_viz::allocate_trace()
{
  return new graphviz_trace;
}

void
graph_viz::delete_trace(void **tr)
{
  graphviz_trace* gtr = reinterpret_cast<graphviz_trace*>(tr);
  delete[] gtr;
}

void
graph_viz::reduce(stat_collector *coll)
{
}

void
graph_viz::clear()
{
}

void
graph_viz::count_trace(long count, thread* thr)
{
#if !SSTMAC_HAVE_GRAPHVIZ
  return; //nothing to do here
#endif


  /** see how much of the backtrace is new.  we don't
      want to double count the number of calls */

  void** stack = thr->backtrace();

  //this stack is actually backwards - oldest call is index 0
  //most recent call is at the end of the list

  int last_collect_nfxn = thr->last_backtrace_nfxn();
  int nfxn_total = thr->backtrace_nfxn();
  if (nfxn_total == 0){
    spkt_abort_printf("graphviz thread %d has no backtrace to collect"
     " - ensure that at least main exists with SSTMACBacktrace",
     thr->thread_id());
  }
  int stack_end = nfxn_total - 1;
  int recollect_stop = std::min(stack_end,last_collect_nfxn);
  for (int i=0; i < recollect_stop; ++i) {
    void* fxn = stack[i];
    void* callfxn = stack[i+1];
    add_call(0, count, fxn, callfxn);
  }

  for (int i=recollect_stop; i < stack_end; ++i) {
    void* fxn = stack[i];
    void* callfxn = stack[i+1];
    add_call(1, count, fxn, callfxn);
  }

  void* fxn = stack[stack_end];
  trace* tr = traces_[fxn];
  if (!tr) {
    tr = new trace(this, fxn);
    traces_[fxn] = tr;
  }
  tr->add_self(count);

  thr->collect_backtrace(nfxn_total);
}

}
}



