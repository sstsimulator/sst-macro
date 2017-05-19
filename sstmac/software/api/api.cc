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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/api/api.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords("host_compute_modeling");

namespace sstmac {
namespace sw {

static thread_lock the_api_lock;

void
api_lock() {
  the_api_lock.lock();
}

void
api_unlock() {
  the_api_lock.unlock();
}

api*
static_get_api(const char *name)
{
  api* a = operating_system::current_thread()->_get_api(name);
  return a;
}

api::~api()
{
  if (hostcompute_) {
    delete timer_;
  }
}

void
api::init(sprockit::sim_parameters* params)
{
  hostcompute_ = params->get_optional_bool_param("host_compute_modeling", false);
  if (hostcompute_) {
    timer_ = new Timer();
    compute_ = operating_system::current_thread()->parent_app()->compute_time_lib();
  }
}

void
api::start_api_call()
{
  if (hostcompute_) {
    if (endcount_ > 0
        && startcount_ == endcount_) { //can't do it on the first time through
      timer_->start();

      timestamp t(timer_->getTime());
      compute_->compute(t);
    }
    startcount_++;
  }
}
void
api::end_api_call()
{
  if (hostcompute_) {
    timer_->stamp();
    endcount_++;
  }
}

timestamp
api::now() const 
{
  return os()->now();
}

void
api::schedule(timestamp t, event_queue_entry* ev)
{
  os()->schedule(t, ev);
}

void
api::schedule_delay(timestamp t, event_queue_entry* ev)
{
  os()->schedule_delay(t, ev);
}

Timer::Timer()
{

#if defined(_MAC)
  mach_timebase_info_data_t info;
  mach_timebase_info(&info);

  conv_factor_ = (static_cast<double> (info.numer))
                / (static_cast<double> (info.denom));
  conv_factor_ = conv_factor_ * 1.0e-9;

#else
  conv_factor_ = 1.0;
#endif

  reset();
}

inline void
Timer::start()
{

#if defined(_MAC)
  start_ = mach_absolute_time();

#else
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts_);
  start_ = static_cast<double>(ts_.tv_sec) + 1.0e-9 *
          static_cast<double>(ts_.tv_nsec);

#endif
}

inline void
Timer::stamp()
{
#if defined(_MAC)
  duration_ = static_cast<double> (mach_absolute_time() - start_);

#else
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts_);
  duration_ = (static_cast<double>(ts_.tv_sec) + 1.0e-9 *
              static_cast<double>(ts_.tv_nsec)) - start_;

#endif

  elapsed_time_ = duration_ * conv_factor_;
}

}
}