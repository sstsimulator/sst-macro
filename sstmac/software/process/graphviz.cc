#include <sstream>

#include <sstmac/software/process/graphviz.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

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

SpktRegister("graph_viz", stat_collector, graph_viz);

graph_viz_increment_stack::graph_viz_increment_stack(const char *fxn, bool skip) :
  skip_(skip)
{
  if (skip)
    return;

  //if (operating_system::current_os()->current_threadid() == thread::main_thread){
  //  spkt_throw(sprockit::illformed_error, "executing backtrace from the main DES thread");
  //}

  thread* thr = operating_system::current_thread();
  if (thr) {
    thr->append_backtrace(const_cast<char*>(fxn));
  }
}

graph_viz_increment_stack::~graph_viz_increment_stack()
{
  if (skip_)
    return;

  thread* thr = operating_system::current_thread();
  if (thr) {
    thr->pop_backtrace();
  }
}

void
graph_viz::trace::add_call(void* fxn, int ncalls, long count)
{
  graphviz_call& call = calls_[fxn];
  call.first += ncalls;
  call.second += count;
}

void
graph_viz::trace::add_self(long count)
{
  self_ += count;
}

void
graph_viz::global_reduce(parallel_runtime *rt)
{
  spkt_throw(sprockit::unimplemented_error, "graph_viz::global_reduce");
}

std::string
graph_viz::trace::summary() const
{
  std::stringstream sstr;
  sstr << "fn=" << (const char*) fxn_ << "\n";
  sstr << 0 << " " << self_ << "\n";
  std::map<void*, graphviz_call>::const_iterator it, end = calls_.end();
  for (it = calls_.begin(); it != end; ++it) {
    const char* fxn = (const char*) it->first;
    const graphviz_call& call = it->second;
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

  std::map<void*,trace*>::const_iterator it, end = traces_.end();
  for (traces_.begin(); it != end; ++it) {
    myfile << it->second->summary();
    myfile << "\n";
  }
  myfile.close();
}

void
graph_viz::simulation_finished(timestamp t)
{
}

graph_viz::graph_viz()
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
  spkt_throw(sprockit::unimplemented_error, "graph_viz::reduce");
}

void
graph_viz::clear()
{
  spkt_throw(sprockit::unimplemented_error, "graph_viz::clear");
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



