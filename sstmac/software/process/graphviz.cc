/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

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
#include <cinttypes>

#if SSTMAC_HAVE_VALID_MPI
#include <mpi.h>
#endif

#if SSTMAC_HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#if SSTMAC_HAVE_DLFCN_H
#include <dlfcn.h>
#endif

namespace sstmac {
namespace sw {

int graph_viz_registration::id_count = 0;
std::map<int,const char*>* graph_viz_registration::names = nullptr;

static sprockit::need_delete_statics<graph_viz> del_statics;

graph_viz_increment_stack::graph_viz_increment_stack(int id)
{
  thread* thr = operating_system::current_thread();
  if (thr) {
    thr->append_backtrace(id);
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

bool
graph_viz::trace::include() const
{
  int ncalls = graph_viz_registration::numIds();
  for (int id=0; id < ncalls; ++id){
    if (calls_[id].counts){
      return true;
    }
  }
  return self_;
}

std::string
graph_viz::trace::summary(const char* fxn) const
{
  std::stringstream sstr;
  int ncalls = graph_viz_registration::numIds();
  sstr << "fn=" << (const char*) fxn << "\n";
  sstr << 0 << " " << self_ << "\n";
  for (int id=0; id < ncalls; ++id){
    if (calls_[id].counts){
      const char* fxn = graph_viz_registration::name(id);
      sstr << "cfn=" << fxn << "\n";
      sstr << "calls=" << calls_[id].ncalls << " " << 0 << "\n";
      sstr << 0 << " " << calls_[id].counts << "\n";
    }
  }
  return sstr.str();
}

graph_viz_registration::graph_viz_registration(const char* name, int id)
{
  if (!names){
    names = new std::map<int,const char*>;
  }
  //*id = id_count++;
  (*names)[id] = name;
}

graph_viz::graph_viz(sprockit::sim_parameters* params) :
  stat_collector(params)
{
  int nfxns = graph_viz_registration::numIds();
  auto trace_size = 1 + 2*nfxns;
  auto total_size = trace_size * nfxns;
  data_block_ = new uint64_t[total_size];
  ::memset(data_block_, 0, total_size*sizeof(uint64_t));
  uint64_t* ptr = data_block_;
  traces_ = new trace*[nfxns];
  for (int i=0; i < nfxns; ++i, ptr += trace_size){
    trace* tr = new (ptr) trace;
    traces_[i] = tr;
  }
}

void
graph_viz::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1){
    return;
  }

  int nfxns = graph_viz_registration::numIds();
  auto trace_size = 1 + 2*nfxns;
  auto total_size = trace_size * nfxns;

#if SSTMAC_HAVE_VALID_MPI
  MPI_Allreduce(MPI_IN_PLACE, data_block_, total_size, MPI_UINT64_T, MPI_SUM, MPI_COMM_WORLD);
#else
  sprockit::abort("cannot support global reduce with MPI");
#endif
}

void
graph_viz::dump_summary(std::ostream& os)
{
  os << "Call Graph Summary\n";
  int nfxns = graph_viz_registration::numIds();
  for (int i=0; i < nfxns; ++i){
    trace* tr = traces_[i];
    if (tr->include()){
      const char* name = graph_viz_registration::name(i);
      int len = ::strlen(name);
      os << name;
      for (int i=0; i < (50 - len); ++i){
        os << " ";
      }
      uint64_t total = tr->self_;
      for (int i=0; i < nfxns; ++i){
        total += tr->calls_[i].counts;
      }
      os << sprockit::printf("%-16" PRIu64 " %-16" PRIu64 "\n", total, tr->self_);
      for (int i=0; i < nfxns; ++i){
        graphviz_call& call = tr->calls_[i];
        if (call.counts){
          const char* name = graph_viz_registration::name(i);
          os << "     ";
          int len = ::strlen(name);
          os << name;
          for (int i=0; i < (45 - len); ++i){
            os << " ";
          }
          os << call.counts << "\n";
        }
      }
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

  int nfxns = graph_viz_registration::numIds();
  for (int i=0; i < nfxns; ++i){
    trace* tr = traces_[i];
    if (tr->include()){
      const char* fxn = graph_viz_registration::name(i);
      myfile << tr->summary(fxn);
      myfile << "\n";
    }
  }
  myfile.close();

  dump_summary(std::cout);
}

void
graph_viz::add_call(
  int ncalls,
  uint64_t count,
  int fxnId,
  int callFxnId
)
{
  trace* tr = traces_[fxnId];
  tr->add_call(callFxnId, ncalls, count);
}

void
graph_viz::delete_statics()
{
}

graph_viz::~graph_viz()
{
}

void
graph_viz::reduce(stat_collector *coll)
{
  graph_viz* other = dynamic_cast<graph_viz*>(coll);
  int nfxns = graph_viz_registration::numIds();
  auto trace_size = 1 + 2*nfxns;
  auto total_size = trace_size * nfxns;
  for (int i=0; i < total_size; ++i){
    data_block_[i] += other->data_block_[i];
  }
}

void
graph_viz::clear()
{
}

void
graph_viz::add_self(int fxnId, uint64_t count)
{
  traces_[fxnId]->add_self(count);
}

void
graph_viz::count_trace(uint64_t count, thread* thr)
{
#if !SSTMAC_HAVE_GRAPHVIZ
  return; //nothing to do here
#else
  /** see how much of the backtrace is new.  we don't
      want to double count the number of calls */

  const int* stack = thr->backtrace();

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
    int fxn = stack[i];
    int callfxn = stack[i+1];
    add_call(0, count, fxn, callfxn);
  }

  for (int i=recollect_stop; i < stack_end; ++i) {
    int fxn = stack[i];
    int callfxn = stack[i+1];
    add_call(1, count, fxn, callfxn);
  }

  int fxn = stack[stack_end];
  add_self(fxn, count);

  thr->collect_backtrace(nfxn_total);
#endif
}

void
graph_viz::reassign(int fxnId, uint64_t count, thread* thr)
{
#if SSTMAC_HAVE_GRAPHVIZ
  int nfxn_total = thr->backtrace_nfxn();
  int stack_end = nfxn_total - 1;
  int fxn = thr->backtrace()[stack_end];
  traces_[fxn]->reassign_self(fxnId, count);
  traces_[fxnId]->add_self(count);
#endif
}

}
}
