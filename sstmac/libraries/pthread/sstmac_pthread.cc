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

#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/operating_system.h>
#include <errno.h>

#include <sstmac/libraries/pthread/sstmac_pthread_runner.h>
#include <sstmac/libraries/pthread/sstmac_pthread_impl.h>

#include <sprockit/unordered.h>

using namespace sstmac;
using namespace sstmac::sw;


DeclareDebugSlot(pthread)
RegisterDebugSlot(pthread)

#define pthread_debug(...) debug_printf(sprockit::dbg::pthread, __VA_ARGS__)

thread*
current_thread()
{
  //this can be null in certain situations
  thread* t = operating_system::current_thread();
  return t;
}

/**
 * Notes:
 *   - attr currently not used
 */
extern "C" int
SSTMAC_pthread_create(sstmac_pthread_t* pthread,
                      const sstmac_pthread_attr_t * attr, void *
                      (*start_routine)(void *), void * arg)
{
  pthread_debug("pthread_create");
  thread* thr = current_thread();
  operating_system* os = thr->os();
  app* parent_app = thr->parent_app();

  thread_id unknown_thrid(-1);
  software_id newid(parent_app->aid(), parent_app->tid(), unknown_thrid);
  pthread_runner* tr = new pthread_runner(newid,
                           parent_app,
                           start_routine, arg, os);

  parent_app->add_subthread(tr);
  *pthread = tr->thread_id();

  if (attr){
    tr->set_cpumask(attr->cpumask);
  }
  
  pthread_debug("starting pthread %ld on thread %ld in app %d",
               tr->thread_id(), thr->thread_id(), int(parent_app->tid()));
  os->start_thread(tr);

  return 0;
}

extern "C" int
SSTMAC_pthread_yield()
{
  pthread_debug("pthread_yield");
  //for now current behavior is to not yield
  return 0;
}

extern "C" void
SSTMAC_pthread_exit(void *retval)
{
  pthread_debug("pthread_exit");
  thread* current = current_thread();
  current->kill();
}

extern "C" int
SSTMAC_pthread_kill(sstmac_pthread_t thread, int sig)
{
  spkt_throw(sprockit::unimplemented_error, "pthread_kill");
}

/**
 * Notes:
 *      - status currently not set, don't use it
 */
extern "C" int
SSTMAC_pthread_join(sstmac_pthread_t pthread, void ** status)
{
  pthread_debug("pthread_join");
  thread* current_thr = current_thread();
  operating_system* os = current_thr->os();
  app* parent_app = os->current_thread()->parent_app();
  thread* joiner = parent_app->get_subthread(pthread);

  pthread_debug("joining pthread %ld on thread %ld in app %d",
               pthread, current_thr->thread_id(), int(parent_app->tid()));

  if (joiner){
    os->join_thread(joiner);
  } else {
    //the thread has already finished - no need to join
  }

  parent_app->remove_subthread(pthread);
  return 0;
}

extern "C" sstmac_pthread_t
SSTMAC_pthread_self()
{
  pthread_debug("pthread_self");
  thread* t = current_thread();
  return t->thread_id();
}

extern "C" int
SSTMAC_pthread_equal(sstmac_pthread_t thread_1, sstmac_pthread_t thread_2)
{
  pthread_debug("pthread_equal");
  if (thread_1 == thread_2){
    return 1;
  } else {
    return 0;
  }
}

extern "C" int
SSTMAC_pthread_spin_init(sstmac_pthread_spinlock_t* lock, int pshared)

{
  return SSTMAC_pthread_mutex_init(lock, NULL);  
}

extern "C" int
SSTMAC_pthread_mutex_init(sstmac_pthread_mutex_t * mutex,
                          const sstmac_pthread_mutexattr_t *attr)
{
  pthread_debug("pthread_mutex_init");
  thread* thr = current_thread();
  app* parent_app = thr->parent_app();
  *mutex = parent_app->allocate_mutex();
  pthread_debug("initialized mutex %d for thread %ld app %d",
                *mutex, thr->thread_id(), int(parent_app->tid()));
  return 0;
}

extern "C" int
SSTMAC_pthread_spin_destroy(sstmac_pthread_spinlock_t * lock)
{
  return SSTMAC_pthread_mutex_destroy(lock);
}

extern "C" int
SSTMAC_pthread_mutex_destroy(sstmac_pthread_mutex_t * mutex)
{
  pthread_debug("pthread_mutex_destroy");
  if (*mutex == SSTMAC_PTHREAD_MUTEX_INITIALIZER)
    return 0; //nothing to do here


  thread* thr = current_thread();
  app* a = thr->parent_app();
  bool found = a->erase_mutex(*mutex);
  if (found){
    return EINVAL;
  } else {
    return 0;
  }

}

static int
check_mutex(sstmac_pthread_mutex_t* mutex)
{
  if (*mutex == SSTMAC_PTHREAD_MUTEX_INITIALIZER){
    return SSTMAC_pthread_mutex_init(mutex, NULL);
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_spin_lock(sstmac_pthread_spinlock_t * lock)
{
  return SSTMAC_pthread_mutex_lock(lock);
}

extern "C" int
SSTMAC_pthread_mutex_lock(sstmac_pthread_mutex_t* mutex)
{
  pthread_debug("pthread_mutex_lock");
  int rc;
  if ((rc = check_mutex(mutex)) != 0){
    return rc;
  }

  thread* thr = current_thread();
  pthread_debug("locking mutex %d for thread %ld for app %d",
                *mutex, thr->thread_id(), int(thr->parent_app()->tid()));
  mutex_t* mut = thr->parent_app()->get_mutex(*mutex);
  if (mut == 0){
    return EINVAL;
  } else if (mut->locked) {
    key* blocker = key::construct();
    mut->waiters.push_back(blocker);
    thr->os()->block(blocker);
  } else {
    mut->locked = true;
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_spin_trylock(sstmac_pthread_spinlock_t * lock)
{
  return SSTMAC_pthread_mutex_trylock(lock);
}


extern "C" int
SSTMAC_pthread_mutex_trylock(sstmac_pthread_mutex_t * mutex)
{
  pthread_debug("pthread_mutex_trylock");  
  int rc;
  if ((rc = check_mutex(mutex)) != 0){
    return rc;
  }

  thread* thr = current_thread();
  mutex_t* mut = thr->parent_app()->get_mutex(*mutex);
  if (mut == 0){
    return EINVAL;
  } else if (mut->locked){
    return 1;
  } else {
    return 0;
  }
}

extern "C" int
SSTMAC_pthread_spin_unlock(sstmac_pthread_spinlock_t * lock)
{
  return SSTMAC_pthread_mutex_unlock(lock);
}

extern "C" int
SSTMAC_pthread_mutex_unlock(sstmac_pthread_mutex_t * mutex)
{
  pthread_debug("pthread_mutex_unlock");  
  int rc;
  if ((rc = check_mutex(mutex)) != 0){
    return rc;
  }

  thread* thr = current_thread();
  mutex_t* mut = thr->parent_app()->get_mutex(*mutex);
  if (mut == 0 || !mut->locked){
    return EINVAL;
  } else if (!mut->waiters.empty()){
    key* blocker = mut->waiters.front();
    mut->waiters.pop_front();
    thr->os()->unblock(blocker);
  } else {
    mut->locked = false;
  }
  return 0;
}

class unblock_event : public event_queue_entry
{
 public:
  unblock_event(mutex_t* mut, operating_system* os)
    : mutex_(mut),
    os_(os),
    event_queue_entry(os->event_location(), os->event_location())
  {
  }

  void
  execute(){
    key* blocker = mutex_->waiters.front();
    mutex_->waiters.pop_front();
    os_->unblock(blocker);
  }

 protected:
  mutex_t* mutex_;

  operating_system* os_;

};


extern "C" int
SSTMAC_pthread_mutexattr_init(sstmac_pthread_mutexattr_t *attr)
{
  pthread_debug("pthread_mutexattr_init");
  return 0; //no op for now
}

extern "C" int
SSTMAC_pthread_mutexattr_destroy(sstmac_pthread_mutexattr_t *attr)
{
  pthread_debug("pthread_mutexattr_destroy");
  return 0; //no op for now
}

extern "C" int
SSTMAC_pthread_cond_init(sstmac_pthread_cond_t * cond,
                         const sstmac_pthread_cond_attr *attr)
{
  pthread_debug("pthread_cond_init");  
  //just intialize it
  thread* thr = current_thread();
  *cond = thr->parent_app()->allocate_condition();
  return 0; //no op for now
}

static int
check_cond(sstmac_pthread_cond_t* cond)
{
  if (*cond == SSTMAC_PTHREAD_COND_INITIALIZER){
    return SSTMAC_pthread_cond_init(cond, NULL);
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_cond_destroy(sstmac_pthread_cond_t * cond)
{
  pthread_debug("pthread_cond_destroy");    
  if (*cond == SSTMAC_PTHREAD_COND_INITIALIZER)
    return 0;

  thread* thr = current_thread();
  thr->parent_app()->erase_condition(*cond);
  return 0;
}

extern "C" int
SSTMAC_pthread_cond_wait(sstmac_pthread_cond_t * cond,
                         sstmac_pthread_mutex_t * mutex)
{
  pthread_debug("pthread_cond_wait"); 
  return SSTMAC_pthread_cond_timedwait(cond, mutex, 0);
}

extern "C" int
SSTMAC_pthread_cond_timedwait(sstmac_pthread_cond_t * cond,
                              sstmac_pthread_mutex_t * mutex, const timespec * abstime)
{
  pthread_debug("pthread_cond_timedwait");  
  int rc;
  if ((rc=check_cond(cond)) != 0){
    return rc;
  }

  thread* thr = current_thread();
  condition_t* pending = thr->parent_app()->get_condition(*cond);
  if (pending == 0){
    return EINVAL;
  }

  mutex_t* mut = thr->parent_app()->get_mutex(*mutex);
  if (mut == 0){
    return EINVAL;
  }

  operating_system* myos = thr->os();
  (*pending)[*mutex] = mut;
  if (!mut->waiters.empty()){
    myos->send_now_self_event_queue(new unblock_event(mut, myos));
  }
  else {
    //unlock - nobody waiting on this
    mut->locked = false;
  }
  key* k = key::construct();
  mut->conditionals.push_back(k);

  if (abstime){
    //schedule a timed wait unblock
    double secs = abstime->tv_sec + 1e-9*abstime->tv_nsec;
    timestamp delay(secs);
    //this key may be used to unblock twice
    //if it does, make sure it doesn't get deleted
    myos->schedule_timeout(delay, k);
  }

  myos->block(k);

  if (abstime && k->timed_out()){
    //the cond signal never fired, remove myself from the list
    //delete the key and move on
    std::list<key*>::iterator it, end = mut->conditionals.end();
    for (it=mut->conditionals.begin(); it != end; ++it){
      key* next = *it;
      if (next == k){
        mut->conditionals.erase(it);
        break;
      }
    }
    delete k;
  }

  return 0;

}

extern "C" int
SSTMAC_pthread_cond_signal(sstmac_pthread_cond_t * cond)
{
  pthread_debug("pthread_cond_signal");
  int rc;
  if ((rc=check_cond(cond)) != 0){
    return rc;
  }

  thread* thr = current_thread();
  condition_t* pending = thr->parent_app()->get_condition(*cond);
  if (pending == 0){
    return EINVAL;
  }
  condition_t::iterator it, end = pending->end();
  operating_system* myos = thr->os();
  for (it=pending->begin(); it != end; ++it){
    mutex_t* mut = it->second;
    if (mut->conditionals.empty()){
        spkt_throw(sprockit::illformed_error,
            "pthread::mutex signaled, but there are no waiting threads");
    }
    key* k = mut->conditionals.front();
    mut->conditionals.pop_front();
    mut->locked = true;
    myos->unblock(k);
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_cond_broadcast(sstmac_pthread_cond_t * cond)
{
  pthread_debug("pthread_cond_broadcast");  
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_cond_broadcast not implemented");
}

extern "C" int
SSTMAC_pthread_once(sstmac_pthread_once_t * once_init, void
                    (*init_routine)(void))
{
  pthread_debug("pthread_once");  
  int boolean = *once_init;
  if (boolean == 0){
    *once_init = 0;
    (*init_routine)();
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_key_create(sstmac_pthread_key_t * key, void
                          (*dest_routine)(void *))
{
  pthread_debug("pthread_key_create");
  app* current = current_thread()->parent_app();
  *key = current->allocate_tls_key(dest_routine);
  return 0;
}

extern "C" int
SSTMAC_pthread_key_delete(sstmac_pthread_key_t key)
{
  pthread_debug("pthread_key_delete");  
  //this is really a no-op
  return 0;
}

extern "C" int
SSTMAC_pthread_setspecific(sstmac_pthread_key_t key, const void * pointer)
{
  pthread_debug("pthread_setspecific");  
  thread* current = current_thread();
  current->set_tls_value(key, const_cast<void*>(pointer));
  return 0;
}

extern "C" void *
SSTMAC_pthread_getspecific(sstmac_pthread_key_t key)
{
  pthread_debug("pthread_getspecific");  
  thread* current = current_thread();
  if (current == 0){
    //why the hell are you calling this function?
    //are you in cxa finalize? write better code
    //so I don't have to make SST cover your mistakes
    return NULL;
  }

  return current->get_tls_value(key);
}

extern "C" void
SSTMAC_pthread_cleanup_push(void
                            (*routine)(void *), void *routine_arg)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_cleanup_push not implemented");
}

extern "C" void
SSTMAC_pthread_cleanup_pop(int execute)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_cleanup_pop not implemented");
}

extern "C" int
SSTMAC_pthread_attr_init(sstmac_pthread_attr_t *attr)
{
  //set all cpus to possibly active
  attr->cpumask = 0;
  attr->cpumask = ~(attr->cpumask);
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_destroy(sstmac_pthread_attr_t *attr)
{
  return 0; //no op for now
}

extern "C" int
SSTMAC_pthread_attr_setaffinity_np(sstmac_pthread_attr_t *attr, size_t cpusetsize, const sstmac_cpu_set_t *cpuset)
{
  attr->cpumask = cpuset->cpubits;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_getaffinity_np(sstmac_pthread_attr_t attr, size_t cpusetsize, sstmac_cpu_set_t *cpuset)
{
  cpuset->cpubits = attr.cpumask;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_getstack(sstmac_pthread_attr_t *attr, void **stack, size_t *stacksize)
{
  *stack = nullptr;
  *stacksize = sstmac::sw::operating_system::stacksize();
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_getstacksize(sstmac_pthread_attr_t *attr,
                                 size_t * stacksize)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_attr_getstacksize not implemented");
}

extern "C" int
SSTMAC_pthread_attr_setstacksize(sstmac_pthread_attr_t *attr, size_t stacksize)
{
  return EPERM;
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_attr_setstacksize not implemented");
}

extern "C" int
SSTMAC_pthread_attr_getdetachstate(const sstmac_pthread_attr_t *attr,
                                   int *state)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_getdetachstate not implemented");
}

extern "C" int
SSTMAC_pthread_attr_setdetachstate(sstmac_pthread_attr_t *attr, int state)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                   "pthread::pthread_setdetachstate not implemented");
}

int
SSTMAC_pthread_attr_setscope(sstmac_pthread_attr_t*, int scope)
{
  if (scope == PTHREAD_SCOPE_PROCESS)
    return ENOTSUP;
  else
    return 0;
}

int
SSTMAC_pthread_attr_getscope(sstmac_pthread_attr_t*, int* scope)
{
  *scope = PTHREAD_SCOPE_SYSTEM;
  return 0;
}

int
SSTMAC_pthread_detach(sstmac_pthread_t thr)
{
  //there is no reason to do anything -
  //all resources associated with this thread
  //will be released back automatically anyway
  return 0;
}