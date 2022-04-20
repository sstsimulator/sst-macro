/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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

#include <sstmac/common/sstmac_config.h>


#include <sstream>
#include <sstmac/software/process/backtrace.h>
#include <sprockit/statics.h>
#include <sprockit/basic_string_tokenizer.h>
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

int CallGraphRegistration::id_count = 0;
std::unique_ptr<std::map<int,const char*>> CallGraphRegistration::names;

CallGraphRegistration::CallGraphRegistration(const char* name, int id)
{
  if (!names){
    names = std::unique_ptr<std::map<int,const char*>>(new std::map<int,const char*>);
  }
  //*id = id_count++;
  (*names)[id] = name;
}

#if !SSTMAC_INTEGRATED_SST_CORE
#if SSTMAC_HAVE_CALL_GRAPH
CallGraphIncrementStack::CallGraphIncrementStack(int id)
{
  Thread* thr = OperatingSystem::currentThread();
  if (thr) {
    thr->appendBacktrace(id);
  } else {
   spkt_abort_printf("graphviz: operating system has no current thread");
  }
}

CallGraphIncrementStack::~CallGraphIncrementStack()
{
  Thread* thr = OperatingSystem::currentThread();
  if (thr) {
    thr->popBacktrace();
  }
}
#endif

bool
CallGraph::FunctionTrace::include() const
{
  int ncalls = CallGraphRegistration::numIds();
  for (int id=0; id < ncalls; ++id){
    if (calls_[id].time_ticks){
      return true;
    }
  }
  return self_time_ticks_;
}

std::string
CallGraph::FunctionTrace::summary(const char* fxn) const
{
  std::stringstream sstr;
  int ncalls = CallGraphRegistration::numIds();
  sstr << "fn=" << (const char*) fxn << "\n";
  sstr << 0 << " " << self_time_ticks_ << "\n";
  for (int id=0; id < ncalls; ++id){
    if (calls_[id].time_ticks){
      const char* fxn = CallGraphRegistration::name(id);
      sstr << "cfn=" << fxn << "\n";
      sstr << "calls=" << calls_[id].ncalls << " " << 0 << "\n";
      sstr << 0 << " " << calls_[id].time_ticks << "\n";
    }
  }
  return sstr.str();
}

CallGraph::CallGraph(SST::BaseComponent *comp, const std::string &name,
                   const std::string &subName, SST::Params& params)
  : SST::Statistics::CustomStatistic(comp, name, subName, params)
{
  int nfxns = CallGraphRegistration::numIds();
  traces_.resize(nfxns, FunctionTrace{nfxns});
}

void
CallGraph::addCall(
  int ncalls,
  uint64_t count,
  int fxnId,
  int callFxnId
)
{
  traces_[fxnId].addCall(callFxnId, ncalls, count);
}

CallGraph::~CallGraph()
{
}

void
CallGraph::addSelf(int fxnId, uint64_t count)
{
  traces_[fxnId].addSelf(count);
}

void
CallGraph::collect(uint64_t count, Thread* thr)
{
  /** see how much of the backtrace is new.  we don't
      want to double count the number of calls */

  const int* stack = thr->backtrace();

  //this stack is actually backwards - oldest call is index 0
  //most recent call is at the end of the list

  int last_collect_nfxn = thr->lastBacktraceNumFxn();
  int nfxn_total = thr->backtraceNumFxn();
  if (nfxn_total == 0){
    spkt_abort_printf("graphviz thread %d has no backtrace to collect"
     " - ensure that at least main exists with SSTMACBacktrace",
     thr->threadId());
  }

  int stack_end = nfxn_total - 1;
  int recollect_stop = std::min(stack_end,last_collect_nfxn);
  for (int i=0; i < recollect_stop; ++i) {
    int fxn = stack[i];
    int callfxn = stack[i+1];
    //add time, but don't increment the number of calls (0)
    addCall(0, count, fxn, callfxn);
  }

  for (int i=recollect_stop; i < stack_end; ++i) {
    int fxn = stack[i];
    int callfxn = stack[i+1];
    //add time and increment the number of calls (1)
    addCall(1, count, fxn, callfxn);
  }

  int fxn = stack[stack_end];
  addSelf(fxn, count);

  thr->recordLastBacktrace(nfxn_total);
}

void
CallGraph::reassign(int fxnId, uint64_t count, Thread* thr)
{
  int nfxn_total = thr->backtraceNumFxn();
  int stack_end = nfxn_total - 1;
  int fxn = thr->backtrace()[stack_end];
  if (traces_[fxn].selfTime() < count){
    spkt_abort_printf(
     "Bad reassignment on task %d - function %s only has %llu ticks, but %s wants to reassign %llu",
     thr->tid(), CallGraphRegistration::name(fxn), traces_[fxn].selfTime(),
     CallGraphRegistration::name(fxnId), count);
  }

  traces_[fxn].reassignSelf(fxnId, count);
  traces_[fxnId].addSelf(count);
}

CallGraphOutput::CallGraphOutput(SST::Params& params) :
  sstmac::StatisticOutput(params)
{
}

void
CallGraphOutput::startOutputGroup(StatisticGroup *grp)
{
  std::string fname = grp->name + ".csv";
  csv_summary_.open(fname);

  csv_summary_ << "Component,Function,CallFunction,Time";
}

void
CallGraphOutput::dumpCallGraph(CallGraph* cgr)
{
  std::string fname = sprockit::sprintf("%s.%s.callgrind.out",
             cgr->groupName().c_str(), cgr->getStatSubId().c_str());
  std::cout << "Dumping " << fname << std::endl;
  std::ofstream myfile(fname);

  myfile << "events: Instructions\n\n";

  int idx = 0;
  for (auto& tr : cgr->traces()){
    if (tr.include()){
      const char* name = CallGraphRegistration::name(idx);
      myfile << tr.summary(name);
      myfile << "\n";
    }
    ++idx;
  }

  myfile.close();
}

void
CallGraphOutput::dumpSummary(CallGraph* cgr)
{
  int idx = 0;
  for (auto& tr : cgr->traces()){
    if (tr.include()){
      const char* name = CallGraphRegistration::name(idx);
      uint64_t total = tr.selfTime();
      for (auto& call : tr.calls()){
        total += call.time_ticks;
      }
      //csv_summary_ << "\n" << cgr->getStatSubId() << ","
      //       << name << ",total," << total;
      csv_summary_ << "\n" << cgr->getStatSubId() << ","
             << name << ",self," << tr.selfTime();

      int callIdx = 0;
      for (auto& call : tr.calls()){
        if (call.time_ticks){
          const char* callName = CallGraphRegistration::name(callIdx);
          csv_summary_ << "\n" << cgr->getStatSubId() << ","
             << name << "," << callName << "," << call.time_ticks;
        }
        ++callIdx;
      }
    }
    ++idx;
  }
}

void
CallGraphOutput::output(StatisticBase *statistic, bool  /*endOfSimFlag*/)
{
  CallGraph* cgr = dynamic_cast<CallGraph*>(statistic);
  if (!cgr){
    spkt_abort_printf("CallGraphOutput::output: received bad statistic");
  }

  dumpCallGraph(cgr);
  dumpSummary(cgr);
}

void
CallGraphOutput::stopOutputGroup()
{
  csv_summary_.close();
}

#endif

}
}


