/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/sst_event.h>
#include <errno.h>

#include <sstmac/libraries/pthread/sstmac_pthread_runner.h>
#include <sstmac/libraries/pthread/sstmac_pthread_impl.h>

#include <unordered_map>

using namespace sstmac;
using namespace sstmac::sw;


DeclareDebugSlot(pthread)
RegisterDebugSlot(pthread)

#define pthread_debug(...) debug_printf(sprockit::dbg::pthread, __VA_ARGS__)

Thread* currentThread()
{
  //this can be null in certain situations
  Thread* t = OperatingSystem::currentThread();
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
  Thread* thr = currentThread();
  OperatingSystem* os = thr->os();
  App* parent_app = thr->parentApp();

  ThreadId unknown_thrid(-1);
  SoftwareId newid(parent_app->aid(), parent_app->tid(), unknown_thrid);
  PthreadRunner* tr = new PthreadRunner(newid,
                           parent_app,
                           start_routine, arg, os,
                           attr ? attr->detach_state : SSTMAC_PTHREAD_CREATE_JOINABLE);

  parent_app->addSubthread(tr);
  *pthread = tr->threadId();

  if (attr){
    tr->setCpumask(attr->cpumask);
  }
  
  pthread_debug("starting pthread %ld:%p on thread %ld:%p in app %d",
               tr->threadId(), tr, thr->threadId(), thr, int(parent_app->tid()));
  os->startThread(tr);

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
SSTMAC_pthread_exit(void * /*retval*/)
{
  pthread_debug("pthread_exit");
  Thread* current = currentThread();
  current->kill();
}

extern "C" int
SSTMAC_pthread_kill(sstmac_pthread_t  /*thread*/, int  /*sig*/)
{
  sprockit::abort("pthread_kill: not implemented");
  return -1;
}

/**
 * Notes:
 *      - status currently not set, don't use it
 */
extern "C" int
SSTMAC_pthread_join(sstmac_pthread_t pthread, void **  /*status*/)
{
  pthread_debug("pthread_join");
  Thread* current_thr = currentThread();
  OperatingSystem* os = current_thr->os();
  App* parent_app = current_thr->parentApp();
  Thread* joiner = parent_app->getSubthread(pthread);

  pthread_debug("joining pthread %ld:%p on thread %ld:%p in app %d",
               pthread, joiner, current_thr->threadId(), current_thr, int(parent_app->tid()));

  if (joiner){
    os->joinThread(joiner);
  } else {
    //the thread has already finished - no need to join
  }

  parent_app->removeSubthread(pthread);
  return 0;
}

extern "C" int
SSTMAC_pthread_testcancel()
{
  sprockit::abort("unimplemented: pthread_testcancel");
  return 0;
}

extern "C" sstmac_pthread_t
SSTMAC_pthread_self()
{
  pthread_debug("pthread_self");
  Thread* t = currentThread();
  return t->threadId();
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
SSTMAC_pthread_mutexattr_gettype(const sstmac_pthread_mutexattr_t*  /*attr*/, int* type)
{
  *type = SSTMAC_PTHREAD_MUTEX_NORMAL;
  return 0;
}

extern "C" int
SSTMAC_pthread_mutexattr_settype(sstmac_pthread_mutexattr_t * /*attr*/, int type)
{
  if (type == SSTMAC_PTHREAD_MUTEX_NORMAL){
    return 0;
  } else {
    return EINVAL;
  }
}

extern "C" int
SSTMAC_pthread_spin_init(sstmac_pthread_spinlock_t* lock, int  /*pshared*/)

{
  return SSTMAC_pthread_mutex_init(lock, NULL);  
}

extern "C" int
SSTMAC_pthread_mutex_init(sstmac_pthread_mutex_t * mutex,
                          const sstmac_pthread_mutexattr_t * /*attr*/)
{
  pthread_debug("pthread_mutex_init");
  Thread* thr = currentThread();
  App* parent_app = thr->parentApp();
  *mutex = parent_app->allocateMutex();
  pthread_debug("initialized mutex %d for thread %ld app %d",
                *mutex, thr->threadId(), int(parent_app->tid()));
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


  Thread* thr = currentThread();
  App* a = thr->parentApp();
  bool found = a->eraseMutex(*mutex);
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

  Thread* thr = currentThread();
  pthread_debug("locking mutex %d for thread %ld for app %d",
                *mutex, thr->threadId(), int(thr->parentApp()->tid()));
  mutex_t* mut = thr->parentApp()->getMutex(*mutex);
  if (mut == 0){
    return EINVAL;
  } else if (mut->locked) {
    mut->waiters.push_back(thr);
    thr->os()->block();
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

  Thread* thr = currentThread();
  mutex_t* mut = thr->parentApp()->getMutex(*mutex);
  if (mut == nullptr){
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

  Thread* thr = currentThread();
  mutex_t* mut = thr->parentApp()->getMutex(*mutex);
  if (mut == 0 || !mut->locked){
    return EINVAL;
  } else if (!mut->waiters.empty()){
    Thread* blocker = mut->waiters.front();
    mut->waiters.pop_front();
    thr->os()->unblock(blocker);
  } else {
    mut->locked = false;
  }
  return 0;
}

class UnblockEvent : public ExecutionEvent
{
 public:
  UnblockEvent(mutex_t* mut, OperatingSystem* os)
    : mutex_(mut),
    os_(os)
  {
  }

  void execute() override{
    Thread* blocker = mutex_->waiters.front();
    mutex_->waiters.pop_front();
    os_->unblock(blocker);
  }

 protected:
  mutex_t* mutex_;

  OperatingSystem* os_;

};


extern "C" int
SSTMAC_pthread_mutexattr_init(sstmac_pthread_mutexattr_t * /*attr*/)
{
  pthread_debug("pthread_mutexattr_init");
  return 0; //no op for now
}

extern "C" int
SSTMAC_pthread_mutexattr_destroy(sstmac_pthread_mutexattr_t * /*attr*/)
{
  pthread_debug("pthread_mutexattr_destroy");
  return 0; //no op for now
}

extern "C" int
SSTMAC_pthread_cond_init(sstmac_pthread_cond_t * cond,
                         const sstmac_pthread_condattr_t * /*attr*/)
{
  pthread_debug("pthread_cond_init");  
  //just intialize it
  Thread* thr = currentThread();
  *cond = thr->parentApp()->allocateCondition();
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

  Thread* thr = currentThread();
  thr->parentApp()->eraseCondition(*cond);
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

  Thread* thr = currentThread();
  condition_t* pending = thr->parentApp()->getCondition(*cond);
  if (pending == 0){
    return EINVAL;
  }

  mutex_t* mut = thr->parentApp()->getMutex(*mutex);
  if (mut == 0){
    return EINVAL;
  }

  OperatingSystem* myos = thr->os();
  (*pending)[*mutex] = mut;
  if (!mut->waiters.empty()){
    myos->sendExecutionEventNow(new UnblockEvent(mut, myos));
  } else {
    //unlock - nobody waiting on this
    mut->locked = false;
  }
  mut->conditionals.push_back(thr);

  if (abstime){
    //schedule a timed wait unblock
    double secs = abstime->tv_sec + 1e-9*abstime->tv_nsec;
    TimeDelta delay(secs);
    //this key may be used to unblock twice
    //if it does, make sure it doesn't get deleted
    myos->blockTimeout(delay);
  } else {
    myos->block();
  }

  if (abstime && thr->timedOut()){
    //the cond signal never fired, remove myself from the list
    //delete the key and move on
    auto end = mut->conditionals.end();
    for (auto it=mut->conditionals.begin(); it != end; ++it){
      Thread* next = *it;
      if (next == thr){
        mut->conditionals.erase(it);
        break;
      }
    }
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

  Thread* thr = currentThread();
  condition_t* pending = thr->parentApp()->getCondition(*cond);
  if (pending == 0){
    return EINVAL;
  }
  condition_t::iterator it, end = pending->end();
  OperatingSystem* myos = thr->os();
  for (it=pending->begin(); it != end; ++it){
    mutex_t* mut = it->second;
    if (mut->conditionals.empty()){
        sprockit::abort("pthread::mutex signaled, but there are no waiting threads");
    }
    Thread* thr = mut->conditionals.front();
    mut->conditionals.pop_front();
    mut->locked = true;
    myos->unblock(thr);
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_cond_broadcast(sstmac_pthread_cond_t *  /*cond*/)
{
  pthread_debug("pthread_cond_broadcast");  
  spkt_throw_printf(sprockit::UnimplementedError,
                   "pthread::pthread_cond_broadcast not implemented");
}

extern "C" int
SSTMAC_pthread_condattr_getpshared(const sstmac_pthread_condattr_t*  /*attr*/, int* pshared)
{
  *pshared = SSTMAC_PTHREAD_PROCESS_PRIVATE;
  return 0;
}

extern "C" int
SSTMAC_pthread_condattr_setpshared(sstmac_pthread_condattr_t*  /*attr*/, int pshared)
{
  if (pshared != SSTMAC_PTHREAD_PROCESS_PRIVATE){
    sprockit::abort("SST does not yet support PTHREAD_PROCESS_SHARED condition");
  }
  return 0;
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
  App* current = currentThread()->parentApp();
  *key = current->allocateTlsKey(dest_routine);
  return 0;
}

extern "C" int
SSTMAC_pthread_key_delete(sstmac_pthread_key_t key)
{
  pthread_debug("delete key %d on tid=%d", key, currentThread()->tid());
  //this is really a no-op
  return 0;
}

extern "C" int
SSTMAC_pthread_setspecific(sstmac_pthread_key_t key, const void * pointer)
{
  Thread* current = currentThread();
  pthread_debug("set key %d to pointer %p on tid=%d",
                key, pointer, current->tid());
  current->setTlsValue(key, const_cast<void*>(pointer));
  return 0;
}

extern "C" void *
SSTMAC_pthread_getspecific(sstmac_pthread_key_t key)
{
  Thread* current = currentThread();
  if (current == nullptr){
    //why are you calling this function?
    //are you in cxa finalize? write better code
    //so I don't have to make SST cover your mistakes
    pthread_debug("no current thread on key=%d", key);
    return NULL;
  }

  void* ptr = current->getTlsValue(key);
  pthread_debug("got key %d to pointer %p on tid=%d",
                key, ptr, current->tid());
  return ptr;
}

extern "C" void
SSTMAC_pthread_cleanup_pop(int  /*execute*/)
{
  sprockit::abort("not implemented: pthread_cleanup_pop");
}

extern "C" void
SSTMAC_pthread_cleanup_push(void (* /*routine*/)(void*), void*  /*arg*/)
{
  sprockit::abort("not implemented: pthread_cleanup_push");
}


extern "C" int
SSTMAC_pthread_attr_init(sstmac_pthread_attr_t *attr)
{
  //set all cpus to possibly active
  attr->cpumask = 0;
  attr->cpumask = ~(attr->cpumask);
  attr->detach_state = SSTMAC_PTHREAD_CREATE_JOINABLE;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_destroy(sstmac_pthread_attr_t * /*attr*/)
{
  return 0; //no op for now
}

extern "C" int
SSTMAC_pthread_attr_setaffinity_np(sstmac_pthread_attr_t *attr, size_t  /*cpusetsize*/, const sstmac_cpu_set_t *cpuset)
{
  attr->cpumask = cpuset->cpubits;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_getaffinity_np(sstmac_pthread_attr_t attr, size_t  /*cpusetsize*/, sstmac_cpu_set_t *cpuset)
{
  cpuset->cpubits = attr.cpumask;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_getstack(sstmac_pthread_attr_t * /*attr*/, void **stack, size_t *stacksize)
{
  *stack = nullptr;
  *stacksize = sstmac::sw::OperatingSystem::stacksize();
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_getstacksize(sstmac_pthread_attr_t * /*attr*/,
                                 size_t *  /*stacksize*/)
{
  spkt_throw_printf(sprockit::UnimplementedError,
                   "pthread::pthread_attr_getstacksize not implemented");
}

extern "C" int
SSTMAC_pthread_attr_setstacksize(sstmac_pthread_attr_t * /*attr*/, size_t  /*stacksize*/)
{
  return EPERM;
  spkt_throw_printf(sprockit::UnimplementedError,
                   "pthread::pthread_attr_setstacksize not implemented");
}

extern "C" int
SSTMAC_pthread_attr_getdetachstate(const sstmac_pthread_attr_t *attr,
                                   int *state)
{
  *state = attr->detach_state;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_setdetachstate(sstmac_pthread_attr_t *attr, int state)
{
  attr->detach_state = state;
  return 0;
}

extern "C" int
SSTMAC_pthread_attr_setscope(sstmac_pthread_attr_t*, int scope)
{
  if (scope == PTHREAD_SCOPE_PROCESS){
    return ENOTSUP;
  } else {
    return 0;
  }
}

extern "C" int
SSTMAC_pthread_attr_getscope(sstmac_pthread_attr_t*, int* scope)
{
  *scope = PTHREAD_SCOPE_SYSTEM;
  return 0;
}

extern "C" int
SSTMAC_pthread_detach(sstmac_pthread_t thr)
{
  App* parent_app = currentThread()->parentApp();
  Thread* joiner = parent_app->getSubthread(thr);
  joiner->setDetachState(Thread::DETACHED);
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_rdlock(sstmac_pthread_rwlock_t * /*rwlock*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_tryrdlock(sstmac_pthread_rwlock_t * /*rwlock*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_wrlock(sstmac_pthread_rwlock_t * /*rwlock*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_trywrlock(sstmac_pthread_rwlock_t * /*rwlock*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_destroy(sstmac_pthread_rwlock_t * /*rwlock*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_init(sstmac_pthread_rwlock_t * /*rwlock*/,
       const sstmac_pthread_rwlockattr_t * /*attr*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlock_unlock(sstmac_pthread_rwlock_t*  /*rwlock*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlockattr_init(sstmac_pthread_rwlockattr_t * /*attr*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_rwlockattr_destroy(sstmac_pthread_rwlockattr_t * /*attr*/)
{
  return 0;
}

extern "C" int
SSTMAC_pthread_setconcurrency(int level)
{
  Thread* thr = currentThread();
  thr->setPthreadConcurrency(level);
  return 0;
}

extern "C" int
SSTMAC_pthread_getconcurrency()
{
  Thread* thr = currentThread();
  return thr->pthreadConcurrency();
}

extern "C" int
SSTMAC_pthread_atfork(void (* /*prepare*/)(void), void (* /*parent*/)(void),
       void (* /*child*/)(void))
{
  sprockit::abort("not implemented: pthread_atfork");
  return 0;
}

extern "C" int
SSTMAC_pthread_mutexattr_getpshared(const sstmac_pthread_mutexattr_t*  /*attr*/, int* pshared)
{
  *pshared = SSTMAC_PTHREAD_PROCESS_PRIVATE;
  return 0;
}

extern "C" int
SSTMAC_pthread_mutexattr_setpshared(sstmac_pthread_mutexattr_t*  /*attr*/, int pshared)
{
  if (pshared != SSTMAC_PTHREAD_PROCESS_PRIVATE){
    sprockit::abort("SST does not yet support PTHREAD_PROCESS_SHARED mutex");
  }
  return 0;
}

extern "C" int
SSTMAC_pthread_setaffinity_np(sstmac_pthread_t thread, size_t cpusetsize, const sstmac_cpu_set_t *cpuset)
{
  Thread* t = OperatingSystem::currentOs()->getThread(thread);
  if (t){
    t->setCpumask(cpuset->cpubits);
    return 0;
  } else {
    return EINVAL;
  }
}

extern "C" int
SSTMAC_pthread_getaffinity_np(sstmac_pthread_t thread, size_t cpusetsize, sstmac_cpu_set_t *cpuset)
{
  Thread* t = OperatingSystem::currentOs()->getThread(thread);
  if (t){
    cpuset->cpubits = t->cpumask();
    return 0;
  } else {
    return EINVAL;
  }
}
