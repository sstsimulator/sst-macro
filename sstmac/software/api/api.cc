/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/api/api.h>
#include <sstmac/hardware/common/flow.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

namespace sstmac {
namespace sw {

static thread_lock the_api_lock;

void
apiLock() {
  the_api_lock.lock();
}

void
apiUnlock() {
  the_api_lock.unlock();
}

API::~API()
{
}

sstmac::sw::SoftwareId
API::sid() const {
  return parent_->sid();
}

sstmac::NodeId
API::addr() const {
  return parent_->os()->addr();
}

Thread*
API::activeThread()
{
  return parent_->os()->activeThread();
}

void
API::startAPICall()
{
  if (host_timer_){
    host_timer_->start();
  }
  activeThread()->startAPICall();
}
void
API::endAPICall()
{
  if (host_timer_) {
    double time = host_timer_->stamp();
    parent_->compute(TimeDelta(time));
  }
  activeThread()->endAPICall();
}

Timestamp
API::now() const 
{
  return parent_->os()->now();
}

void
API::schedule(Timestamp t, ExecutionEvent* ev)
{
  parent_->os()->sendExecutionEvent(t, ev);
}

void
API::scheduleDelay(TimeDelta t, ExecutionEvent* ev)
{
  parent_->os()->sendDelayedExecutionEvent(t, ev);
}

API::API(SST::Params & /*params*/, App *parent, SST::Component*  /*comp*/) :
  host_timer_(nullptr),
  parent_(parent)
{
  //host_timer_(new HostTimer)
}

}
}
