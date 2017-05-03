/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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

