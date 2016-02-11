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

#ifndef SSTMAC_SOFTWARE_PROCESS_APP_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_APP_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/lib_compute_time.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/libraries/compute/lib_compute_loops.h>
#include <sstmac/software/libraries/compute/lib_sleep.h>
#include <sprockit/factories/factory.h>
#include <sstmac/software/process/thread.h>

#include <sstmac/software/api/api_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {
namespace sw {

class mutex_t  {
 public:
  std::string
  to_string() const {
    return "sstmac mutex";
  }

  /** Blocking keys for those threads waiting on the mutex */
  std::list<key*> waiters;
  std::list<key*> conditionals;
  bool locked;

  mutex_t() : locked(false)
  {
  }
};

typedef std::map<long, mutex_t*> condition_t;

/**
 * The app derived class adds to the thread base class by providing
 * facilities to allow applications to simulate computation.
 * Messaging models are supported through an api class,
 * which are stored by the app
 */
class app :
  public thread,
  public sprockit::factory_type
{
 public:
  typedef void (*destructor_fxn)(void*);

  int
  allocate_tls_key(destructor_fxn fnx);

  software_id
  sid() const {
   return id_;
  }

  int
  appnum() const {
    return id_.app_;
  }

  int
  tasknum() const {
    return id_.task_;
  }

  app*
  parent_app() const {
    return const_cast<app*>(this);
  }

  static void
  delete_statics();

  api*
  build_api(int aid, const std::string &name);

  virtual void
  init_os(operating_system* os);

  void
  sleep(timestamp time);

  // convenience functions for apps
  // public to allow C interface
  void
  compute(timestamp time);

  void
  compute_inst(const compute_message::ptr&cmsg);

  void
  compute_loop(long num_loops,
    int nflops_per_loop,
    int nintops_per_loop,
    int bytes_per_loop);

  void
  compute_detailed(long flops, long intops, long bytes);

  void
  compute_block_read(long bytes);

  void
  compute_block_write(long bytes);

  void
  compute_block_memcpy(long bytes);

  lib_compute_loops*
  compute_loops_lib();

  std::string
  compute_name();

  std::string
  compute_inst_name();

  std::string
  compute_memmove_name();

  /// Goodbye.
  virtual ~app();

  char** argv();

  int argc();

  virtual void
  consume_params(sprockit::sim_parameters* params) = 0;

  virtual app*
  clone_type() = 0;

  app*
  clone(software_id newid);

  //called when killing the app, in case you want to check or clean anything up before destructor
  virtual void kill();

  virtual void
  skeleton_main() = 0;

  virtual void run();

  static void
  init_argv(app_id aid, sprockit::sim_parameters* params);

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  sprockit::sim_parameters*
  params() const {
    return params_;
  }

  /**
   * Let a parent application know about the existence of a subthread
   * If thread does not have an initialized ID, a unique ID is allocated for the thread
   * Can be called from a constructor. This method does NOT throw.
   * @param thr
   */
  void add_subthread(thread* thr);

  /**
   * Indicate to parent application that subthread is done running.
   * This puts a null marker for the thread rather than deleting it completely
   * @param thr
   */
  void set_subthread_done(thread* thr);

  /**
   * Let a parent application know a subthread has finished.
   * This completely erases the thread. There will be no record of this thread after calling this function.
   * @param thr A thread with initialized ID
   */
  void remove_subthread(thread* thr);

  void remove_subthread(long thr_id);

  /**
   * @brief get_subthread
   * @param id
   * @return
   */
  thread* get_subthread(long id);

  /**
   * Allocate a unique ID for a mutex variable
   * @return The unique ID
   */
  int allocate_mutex();

  /**
   * Allocate a unique ID for a condition variable
   * @return The unique ID
   */
  int allocate_condition();

  /**
   * Fetch a mutex object corresponding to their ID
   * @param id
   * @return The mutex object corresponding to the ID. Return NULL if no mutex is found.
   */
  mutex_t* get_mutex(int id);

  /**
   * Fetch a condition object corresponding to the ID
   * @param id
   * @return The condition object corresponding to the ID. Return NULL if not condition is found.
   */
  condition_t* get_condition(int id);

  bool erase_condition(int id);

  bool erase_mutex(int id);

  virtual void
  clear_subthread_from_parent_app();

 protected:
  app();

  virtual void init_mem_lib();

 private:
  sprockit::sim_parameters* params_;
  lib_compute_inst* compute_inst_;
  lib_compute_time* compute_time_;
  lib_compute_memmove* compute_mem_move_;
  lib_compute_loops* compute_loops_;
  lib_sleep* sleep_lib_;
  long next_tls_key_;

  software_id id_;

  int next_condition_;
  int next_mutex_;

  static std::vector<char**> argv_for_app_;

  static std::vector<int> argc_for_app_;

  std::map<long, thread*> subthreads_;

  std::map<int, mutex_t> mutexes_;

  std::map<int, condition_t> conditions_;

  std::map<int, destructor_fxn> tls_key_fxns_;

};

/** utility function for computing stuff */
void compute_time(double tsec);

class app_factory : public sprockit::SpktFactory<app>
{

 public:
  static void
  print_apps();

  static void
  clear_apps();

};

}
} // end of namespace sstmac.

#define SpktRegisterApp(appstr, appname, ...) \
    SpktRegister(appstr, ::sstmac::sw::app, appname, __VA_ARGS__)

#define sstmac_register_app(cls) \
static int cls##_main(int argc, char** argv); \
class cls :  \
    public ::sstmac::sw::app \
{ \
 public: \
  cls(){} \
  \
  virtual ~cls() throw () {} \
\
  ::sstmac::sw::app* \
  clone_type() { \
      return new cls; \
  } \
\
  void consume_params(::sprockit::sim_parameters* params) \
  { \
  } \
    \
  std::string \
  to_string() const { \
    return "new skeleton"; \
  } \
\
  void skeleton_main() { \
      cls##_main(argc(), argv()); \
  } \
}; \
SpktRegister(#cls, ::sstmac::sw::app, cls);


#endif

