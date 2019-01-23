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

#ifndef sstmac_software_process_FTQ_H
#define sstmac_software_process_FTQ_H

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/thread_fwd.h>
#include <stdint.h>
#include <vector>

#include <unordered_map>

#include <string>
#include <stdlib.h>

/**
  "Fixed time quanta" collection of the amount of work done in particular time intervals
  over the course of the computation
*/
namespace sstmac {
namespace sw {


class FTQEpoch
{
 private:
  friend class AppFTQCalendar;

  uint64_t* totals_;

 public:
  FTQEpoch();

  virtual ~FTQEpoch();

  void collect(int key_typeid, uint64_t ticks) {
    totals_[key_typeid] += ticks;
  }

  void init(int num_events, uint64_t* buffer);

  uint64_t eventTime(int key_typeid) const {
    return totals_[key_typeid];
  }

  void setEventTime(int key_typeid, uint64_t count) {
    totals_[key_typeid] = count;
  }

};

class AppFTQCalendar
{
 public:
  AppFTQCalendar(int aid, const std::string& appname,
                 uint64_t nticks_epoch);

  void dump(const std::string& fileroot);

  virtual ~AppFTQCalendar();

  /**
    @param event_typeid The type of event (MPI,Compute,Sleep,etc)
    @param tid The task id (essentially MPI Rank)
    @param ticks_begin The time the event started
    @param num_ticks The duration of the event
  */
  void collect(int event_typeid, int tid, uint64_t ticks_begin, uint64_t num_ticks);

  void reduce(AppFTQCalendar* cal);

  void globalReduce(ParallelRuntime* rt);

 private:
  std::vector<FTQEpoch> epochs_;

  FTQEpoch aggregate_;

  std::list<uint64_t*> buffers_;

  void dump(std::ofstream& os);

  int aid_;

  int max_tid_;

  uint64_t max_epoch_;

  uint64_t max_epoch_allocated_;

  uint64_t num_ticks_epoch_;

  std::string appname_;

  void dumpMatplotlibHistogram(const std::string& fileroot);

  void allocateEpochs(uint64_t max_epoch);

  static const uint64_t allocation_num_epochs;

};

class FTQCalendar : public MultiStatistic<int,int,int,uint64_t,uint64_t>
{
  using Parent = MultiStatistic<int,int,int,uint64_t,uint64_t>;
  FactoryRegister("ftq", Parent, FTQCalendar)
 public:
  FTQCalendar(SST::Params& params);

  void init(long nticks_per_epoch);

  virtual ~FTQCalendar();

  void addData_impl(int event_typeid, int aid, int tid,
          uint64_t ticks_begin, uint64_t num_ticks);

  AppFTQCalendar* getCalendar(int aid) const;

  void registerApp(int aid, const std::string& appname);

 private:
  static std::unordered_map<int, AppFTQCalendar*> calendars_;

  uint64_t num_ticks_epoch_;

};

// Ensures an ftq_tag is used for the life of this object
class FTQScope {
public:
    FTQScope(Thread*, FTQTag);
    FTQScope(Thread*);
    ~FTQScope();

private:
    // We don't want dynamic allocations.
    // This class is intended to use scoping for construction/deconstruction
    void* operator new(size_t size) throw();

    // private members
    bool _tag_previously_protected;
    FTQTag _previous_tag;
    Thread* _thread;
};

}
}

#endif // FTQ_H
