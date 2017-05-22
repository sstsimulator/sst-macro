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

#ifndef sstmac_software_libraries_API_H
#define sstmac_software_libraries_API_H

#include <sprockit/factories/factory.h>
#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/key_fwd.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/software/libraries/compute/lib_compute_time.h>

# if  (defined(__MACH__) && defined(__APPLE__))
#   define _MAC
#endif

# if defined(_MAC)
#    include <mach/mach_time.h>
# else
#    include <time.h>
# endif

#if defined(_MAC)
typedef uint64_t my_timer_t;
typedef double timer_c;
#else
typedef double my_timer_t;
typedef timespec timer_c;
#endif

namespace sstmac {
namespace sw {

//==============================================================================
// Timer
// A quick class to do benchmarking.
// Example: Timer t;  t.tic();  SomeSlowOp(); t.toc("Some Message");

class Timer
{
 public:
  Timer();

  /**
   * @brief start
   * Set the start time for the current window
   */
  inline void start();

  /**
   * @brief stamp
   * Set the current time as the endpoint for the current window
   */
  inline void stamp();

  void reset(){
    start_ = 0;
    duration_ = 0;
    elapsed_time_ = 0;
  }

  double getTime() const {
    return elapsed_time_;
  }

 private:
  my_timer_t start_;
  double duration_;
  timer_c ts_;
  double conv_factor_;
  double elapsed_time_;
};

class api :
  public library
{
  DeclareFactory(api,software_id,operating_system*)
 public:
  api(sprockit::sim_parameters* params,
      const char* prefix,
      software_id sid,
      operating_system* os,
      const key_traits::category& ty) :
    api(params, prefix, sid, os)
  {
    key_cat_ = ty;
  }

  api(sprockit::sim_parameters *params,
      const std::string& libname,
      software_id sid,
      operating_system *os) :
    library(libname, sid, os),
    startcount_(0),
    endcount_(0)
  {
    init(params);
  }

  api(sprockit::sim_parameters* params,
      const char* prefix,
      software_id sid,
      operating_system* os) :
    api(params, standard_lib_name(prefix, sid), sid, os)
  {
  }

  virtual ~api();

  virtual void init(){}

  virtual void finish(){}

  timestamp now() const;

  void schedule(timestamp t, event_queue_entry* ev);

  void schedule_delay(timestamp t, event_queue_entry* ev);

  /**
   * @brief start_api_call
   * Enter a call such as MPI_Send. Any perf counters or time counters
   * collected since the last API call can then advance time or
   * increment statistics.
   */
  virtual void start_api_call();

  /**
   * @brief end_api_call
   * Exit a call such as MPI_Send. Perf counters or time counters
   * collected since the last API call can then clear counters for
   * the next time window.
   */
  virtual void end_api_call();

 protected:
  bool hostcompute_;
  Timer* timer_;
  long startcount_;
  long endcount_;
  lib_compute_time* compute_;

 private:
  void init(sprockit::sim_parameters *params);
};

void api_lock();
void api_unlock();

#define NamespaceRegister(name, child_cls)

#define RegisterAPI(name, child_cls) \
  FactoryRegister(name, sstmac::sw::api, child_cls) \
  NamespaceRegister(name, child_cls)

}
}

#endif // API_H