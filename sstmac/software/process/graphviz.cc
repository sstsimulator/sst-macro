/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

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
  graphviz_call& call = calls_[fxn];
  call.first += ncalls;
  call.second += count;
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

void
graph_viz::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() > 1){
    spkt_throw(sprockit::unimplemented_error,
      "graph_viz::global_reduce: graphviz not available in parallel");
  }
}

graph_viz::trace*
graph_viz::get_trace(void* fxn)
{
  return traces_[fxn];
}

void
graph_viz::dump_summary(std::ostream& os)
{
  for (auto& pair : traces_){
    trace* tr = pair.second;
    const char* name = (const char*) pair.first;
    int len = ::strlen(name);
    os << name;
    for (int i=0; i < (50 - len); ++i){
      os << " ";
    }
    uint64_t total = tr->self_;
    for (auto& callpair : tr->calls_){
      total += callpair.second.second;
    }
    os << sprockit::printf("%12lu %12lld\n", total, tr->self_);
    for (auto& callpair : tr->calls_){
      graphviz_call& call = callpair.second;
      const char* name = (const char*)callpair.first;
      os << "     ";
      int len = ::strlen(name);
      os << name;
      for (int i=0; i < (45 - len); ++i){
        os << " ";
      }
      os << call.second << "\n";
    }
  }
}

void
graph_viz::dump_local_data()
{
  char fname[128];
  sprintf(fname, "%s.calls.%d.out", fileroot_.c_str(), id());
  std::ofstream ofs(fname);
  dump_summary(ofs);
  ofs.close();
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

  dump_summary(std::cout);

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
  graph_viz* other = dynamic_cast<graph_viz*>(coll);
  for (auto& trPair : other->traces_){
    void* name = trPair.first;
    trace* othTrace = trPair.second;
    trace* myTrace = traces_[name];
    if (myTrace == nullptr){
      myTrace = new trace(this, name);
      traces_[name] = myTrace;
    }
    myTrace->add_self(othTrace->self_);
    for (auto& callPair : othTrace->calls_){
      void* callName = callPair.first;
      graphviz_call& call = callPair.second;
      myTrace->add_call(callName, call.first, call.second);
    }
  }
}

void
graph_viz::clear()
{
}

void
graph_viz::add_self(void* fxn, long count)
{
  trace* tr = traces_[fxn];
  if (!tr) {
    tr = new trace(this, fxn);
    traces_[fxn] = tr;
  }
  tr->add_self(count);
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

  static thread_lock lock;
  lock.lock();

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
  add_self(fxn, count);
  lock.unlock();

  thr->collect_backtrace(nfxn_total);
}

}
}