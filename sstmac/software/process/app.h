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

  typedef int (*main_fxn)(int argc, char** argv);
  typedef int (*empty_main_fxn)();

  int
  allocate_tls_key(destructor_fxn fnx);

  static sprockit::sim_parameters*
  get_params();

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

  void
  sleep(timestamp time);

  // convenience functions for apps
  // public to allow C interface
  void
  compute(timestamp time);

  void
  compute_inst(compute_event* cmsg);

  void
  compute_loop(uint64_t,
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

  /// Goodbye.
  virtual ~app();

  virtual void
  consume_params(sprockit::sim_parameters* params) = 0;

  virtual app*
  clone_type() const = 0;

  app*
  clone(software_id newid);

  //called when killing the app, in case you want to check or clean anything up before destructor
  virtual void kill();

  virtual void
  skeleton_main() = 0;

  virtual void run();

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
  friend class thread;

  app();

  api*
  _get_api(const char* name);

  virtual void init_mem_lib();

  sprockit::sim_parameters* params_;
  software_id id_;

 private:
  lib_compute_inst* compute_inst_;
  lib_compute_time* compute_time_;
  lib_compute_memmove* compute_mem_move_;
  lib_compute_loops* compute_loops_;
  lib_sleep* sleep_lib_;
  long next_tls_key_;

  int next_condition_;
  int next_mutex_;

  std::map<long, thread*> subthreads_;

  std::map<int, mutex_t> mutexes_;

  std::map<int, condition_t> conditions_;

  std::map<int, destructor_fxn> tls_key_fxns_;

};

class user_app_cxx_full_main : public app
{
 public:
  static void
  register_main_fxn(const char* name, app::main_fxn fxn);

  void skeleton_main();

  virtual void
  consume_params(sprockit::sim_parameters *params);

  static void
  delete_statics();

  app*
  clone_type() const {
    return new user_app_cxx_full_main;
  }

  struct argv_entry {
    char** argv;
    int argc;
    argv_entry() : argv(0), argc(0) {}
  };

 private:
  void init_argv(argv_entry& entry);

  static std::map<std::string, app::main_fxn>* main_fxns_;
  static std::map<app_id, argv_entry> argv_map_;
  app::main_fxn fxn_;

};

class user_app_cxx_empty_main : public app
{
 public:
  static void
  register_main_fxn(const char* name, app::empty_main_fxn fxn);

  virtual void
  consume_params(sprockit::sim_parameters *params);

  app*
  clone_type() const {
    return new user_app_cxx_empty_main;
  }

  void skeleton_main();

 private:
  static std::map<std::string, app::empty_main_fxn>* empty_main_fxns_;
  app::empty_main_fxn fxn_;

};

/** utility function for computing stuff */
void compute_time(double tsec);

DeclareFactory(app)

}
} // end of namespace sstmac.

#endif

