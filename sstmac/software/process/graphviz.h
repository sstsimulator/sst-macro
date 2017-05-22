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
  FactoryRegister("graph_viz | call_graph", stat_collector, graph_viz)
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
  void dump_summary(std::ostream& os);

  typedef std::pair<long, long long> graphviz_call;
  class trace  {
   friend class graph_viz;
   private:
    std::map<void*, graphviz_call> calls_;

    long long self_;

    void* fxn_;

    graph_viz* parent_;

   public:
    trace(graph_viz* parent, void* fxn) :
      self_(0), fxn_(fxn), parent_(parent)
    {
    }

    std::string
    summary() const;

    void* fxn() const {
      return fxn_;
    }

    void add_call(void* fxn, int ncalls, long count);

    void add_self(long count) {
      self_ += count;
    }

    void substract_self(long count) {
      self_ -= count;
    }

  };

  void add_call(int ncalls, long count, void* fxn, void* callfxn);

  void add_self(void* fxn, long count);

  std::map<void*, trace*> traces_;

  std::map<void*, std::string> ptr_to_fxn_;

  std::map<std::string, void*> fxn_to_ptr_;

  friend class trace;

  trace* get_trace(void* fxn);


};

}
}


#endif // GRAPHVIZ_H