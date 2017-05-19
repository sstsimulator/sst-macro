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

#include <sstmac/common/thread_safe_int.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/libraries/library.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/api/api.h>
#include <sstmac/common/sst_event.h>
#include <sprockit/errors.h>
#include <sprockit/output.h>
#include <iostream>
#include <exception>
#include <unistd.h>  // getpagesize()
#include <sys/mman.h>
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>

namespace sstmac {
namespace sw {

static thread_safe_long THREAD_ID_CNT(0);
const app_id thread::main_thread_aid(-1);
const task_id thread::main_thread_tid(-1);

//
// Private method that gets called by the scheduler.
//
void
thread::init_thread(int physical_thread_id, threading_interface* threadcopy, void *stack,
                    int stacksize, threading_interface *yield_to, void* globals_storage)
{
  stack_ = stack;
  stacksize_ = stacksize;

  init_id();

  state_ = INITIALIZED;

  context_ = threadcopy->copy();

  threadinfo* info = new threadinfo();
  info->thethread = this;

  context_->start_context(physical_thread_id, stack, stacksize, run_routine, info,
                          yield_to, globals_storage);
}

device_id
thread::event_location() const
{
  return os_->event_location();
}

thread*
thread::current()
{
  return operating_system::current_thread();
}

api*
thread::_get_api(const char* name)
{
  return parent_app_->_get_api(name);
}

void
thread::clear_subthread_from_parent_app()
{
  //if this is canceled, the parent app might already be dead
  if (parent_app_){
    parent_app_->remove_subthread(this);
  }
}

/**
 * This can get called by anyone to have a thread exit, including during normal app termination
 */
void
thread::kill()
{
  // We are done, ask the scheduler to remove this task from the
  state_ = DONE;

  clear_subthread_from_parent_app();

  // This is a little bit weird - kill is happening on a non-DES thread stack
  os_->complete_thread(true);

  //we will never actually arrive here, instead the os context switches out
}

void
thread::cleanup()
{
}

class delete_thread_event :
  public event_queue_entry
{
 public:
  delete_thread_event(thread* thr) :
    thr_(thr),
    event_queue_entry(thr->os()->event_location(), thr->os()->event_location())
  {
  }

  void
  execute(){
    thr_->cleanup();
    delete thr_;
  }

 protected:
  thread* thr_;
};

//
// Run routine that defines the initial context for this task.
//
void
thread::run_routine(void* threadptr)
{
  threadinfo* info = (threadinfo*) threadptr;
  thread* self = info->thethread;
  delete info;

  // Go.
  if (self->is_initialized()) {
    self->state_ = ACTIVE;
    bool success = false;
    try {
      self->run();
      success = true;
      //JJW 11/6/2014 This here is weird
      //The thread has run out of work and is terminating
      //However, because of weird thread swapping the DES thread
      //might still operate on the thread... we need to delay the delete
      //until the DES thread has completely finished processing its current event
      self->os()->schedule_now(new delete_thread_event(self));
      //this doesn't so much kill the thread as context switch it out
      //it is up to the above delete thread event to actually to deletion/cleanup
      //all of this is happening ON THE THREAD - it kills itself
      //this is not the DES thread killing it
      self->kill();
    }
    catch (const std::exception &ex) {
      cerrn << "thread terminated with exception: " << ex.what()
                << "\n";
      // should forward the exception to the main thread,
      // but for now will abort
      cerrn << "aborting" << std::endl;
      std::cout.flush();
      std::cerr.flush();
      abort();
    }
    catch (const std::string& str) {
      cerrn << "thread terminated with string exception: " << str << "\n";
      cerrn << "aborting" << std::endl;
      std::cout.flush();
      std::cerr.flush();
      abort();
    }
  }
  else {
    spkt_throw(sprockit::os_error,
              "thread::run_routine: task has not been initialized");
  }
}

key_traits::category schedule_delay("CPU_Sched Delay");

thread::thread(sprockit::sim_parameters* params, software_id sid, operating_system* os) :
  os_(os),
  state_(PENDING),
  isInit(false),
  backtrace_(nullptr),
  bt_nfxn_(0),
  last_bt_collect_nfxn_(0),
  thread_id_(thread::main_thread),
  schedule_key_(key::construct(schedule_delay)),
  p_txt_(process_context::none),
  stack_(nullptr),
  context_(nullptr),
  cpumask_(0),
  parent_app_(nullptr),
  sid_(sid)
{
  //make all cores possible active
  cpumask_ = ~(cpumask_);
}

long
thread::init_id()
{
  //thread id not yet initialized
  if (thread_id_ == thread::main_thread)
    thread_id_ = THREAD_ID_CNT++;
  //I have not yet been assigned a process context (address space)
  //make my own, for now
  if (p_txt_ == process_context::none)
    p_txt_ = thread_id_;
  return thread_id_;
}

void*
thread::get_tls_value(long thekey) const
{
  auto it = tls_values_.find(thekey);
  if (it == tls_values_.end())
    return nullptr;
  return it->second;
}

void
thread::set_tls_value(long thekey, void *ptr)
{
  tls_values_[thekey] = ptr;
}

void
thread::append_backtrace(void* fxn)
{
  backtrace_[bt_nfxn_] = fxn;
  bt_nfxn_++;
}

void
thread::pop_backtrace()
{
  --bt_nfxn_;
  last_bt_collect_nfxn_ = std::min(last_bt_collect_nfxn_, bt_nfxn_);
}

void
thread::collect_backtrace(int nfxn)
{
  last_bt_collect_nfxn_ = nfxn;
}

void
thread::spawn(thread* thr)
{
  thr->parent_app_ = parent_app();
  os_->start_thread(thr);
}

timestamp
thread::now()
{
  return os_->now();
}

thread::~thread()
{
  if (backtrace_) graph_viz::delete_trace(backtrace_);
  if (stack_) os_->free_thread_stack(stack_);
  if (context_) {
    context_->destroy_context();
    delete context_;
  }
  if (schedule_key_) delete schedule_key_;
}


void
thread::start_thread(thread* thr)
{
  thr->p_txt_ = p_txt_;
  os_->start_thread(thr);
}


void
thread::join()
{
  if (!this->is_initialized()) {
    // We can't context switch the caller out without first being initialized
    spkt_throw_printf(sprockit::illformed_error,
                     "thread::join: target thread has not been initialized.");
  }
  os_->join_thread(this);
}

}
} // end of namespace sstmac