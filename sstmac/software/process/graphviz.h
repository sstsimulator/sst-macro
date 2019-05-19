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

#ifndef SSTMAC_SOFTWARE_PROCESS_GRAPHVIZ_H
#define SSTMAC_SOFTWARE_PROCESS_GRAPHVIZ_H

#define GraphVizAppendBacktrace(name) \
  struct graph_viz_##name : public sstmac::sw::graph_viz_ID<graph_viz_##name> {}; \
  static sstmac::sw::graph_viz_registration graph_viz_reg(#name, graph_viz_##name::id); \
  ::sstmac::sw::graph_viz_increment_stack __graphviz_tmp_variable__(graph_viz_##name::id)
#define GraphVizDoNothing(...) int __graphviz_tmp_variable__

#define GraphVizCreateTag(name) \
  struct graph_viz_##name : public sstmac::sw::graph_viz_ID<graph_viz_##name> {}; \
  static sstmac::sw::graph_viz_registration graph_viz_reg(#name, graph_viz_##name::id)

#define GraphVizTag(name) graph_viz_##name::id

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/software/process/thread_fwd.h>

namespace sstmac {
namespace sw {

struct graph_viz_registration {
  graph_viz_registration(const char* name, int id);

  static int numIds() {
    return id_count;
  }

  static const char* name(int id){
    auto iter = names->find(id);
    return iter->second;
  }

  static int id_count;

 private:
  static std::unique_ptr<std::map<int,const char*>> names;
};


template <class T>
struct graph_viz_ID {
 public:
  static int id;
};
template <class T> int graph_viz_ID<T>::id = graph_viz_registration::id_count++;

class GraphVizIncrementStack
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
  GraphVizIncrementStack(int id);

  ~GraphVizIncrementStack();

};

class GraphViz :
  public SST::Statistics::MultiStatistic<uint64_t,sw::Thread*>
{
  using Parent=SST::Statistics::MultiStatistic<uint64_t,sw::Thread*>;
 public:
  /**
  SST_ELI_REGISTER_CUSTOM_STATISTIC(
      Parent,
      GraphViz,
      "macro",
      "graph_viz",
      SST_ELI_ELEMENT_VERSION(1,0,0),
      "collect graphviz call trace")
  */

  GraphViz(SST::BaseComponent* comp, const std::string& name,
           const std::string& subName, SST::Params& params);

  virtual ~GraphViz();

  void addData_impl(uint64_t count, sw::Thread* thr) override;

  void reassign(int fxnId, uint64_t count, Thread* thr);

  static void deleteStatics();

 private:
  struct GraphvizCall {
    uint64_t ncalls;
    uint64_t counts;
  };

  class Trace  {
   friend class GraphViz;
   private:
    GraphvizCall calls_[0];

    uint64_t self_;

   public:
    Trace() : self_(0) {}

    std::string summary(const char* fxn) const;

    bool include() const;

    void addCall(int fxnId, int ncalls, uint64_t count) {
      GraphvizCall& call = calls_[fxnId];
      call.ncalls += ncalls;
      call.counts += count;
    }

    void addSelf(uint64_t count) {
      self_ += count;
    }

    void reassignSelf(int fxnId, uint64_t count) {
      self_ -= count;
      GraphvizCall& call = calls_[fxnId];
      call.ncalls += 1;
      call.counts += count;
    }

    void substractSelf(uint64_t count) {
      self_ -= count;
    }

  };

  void addCall(int ncalls, uint64_t count, int fxnId, int callFxnId);

  void addSelf(int fxnId, uint64_t count);

  Trace** traces_;

  uint64_t* data_block_;

  friend class Trace;


};

#define BACKTRACE_NFXN 50
typedef int graphviz_trace[BACKTRACE_NFXN];

}
}


#endif // GRAPHVIZ_H
