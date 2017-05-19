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

#ifndef SSTMAC_SOFTWARE_PROCESS_APP_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_APP_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_fwd.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sprockit/factories/factory.h>
#include <sstmac/software/process/thread.h>

#include <sstmac/software/api/api_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {
namespace sw {

class mutex_t  {
 public:
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
class app : public thread
{
  DeclareFactory(app, software_id, operating_system*)
 public:
  typedef void (*destructor_fxn)(void*);

  typedef int (*main_fxn)(int argc, char** argv);
  typedef int (*empty_main_fxn)();

  int allocate_tls_key(destructor_fxn fnx);

  static sprockit::sim_parameters* get_params();

  app* parent_app() const override {
    return const_cast<app*>(this);
  }

  static void delete_statics();

  void sleep(timestamp time);

  void compute(timestamp time);

  void compute_inst(compute_event* cmsg);

  void compute_loop(uint64_t,
    int nflops_per_loop,
    int nintops_per_loop,
    int bytes_per_loop);

  void compute_detailed(long flops, long intops, long bytes);

  void compute_block_read(long bytes);

  void compute_block_write(long bytes);

  void compute_block_memcpy(long bytes);

  lib_compute_loops* compute_loops_lib();

  lib_compute_time* compute_time_lib();

  /// Goodbye.
  virtual ~app();

  //called when killing the app, in case you want to check or clean anything up before destructor
  virtual void kill() override;

  virtual void skeleton_main() = 0;

  void run() override;

  sprockit::sim_parameters* params() const {
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

  void* globals_storage() const {
    return globals_storage_;
  }

  virtual void clear_subthread_from_parent_app() override;

  const std::string& unique_name() const {
    return unique_name_;
  }

  void set_unique_name(const std::string& name) {
    unique_name_ = name;
  }

 protected:
  friend class thread;

  app(sprockit::sim_parameters *params, software_id sid,
      operating_system* os);

  api* _get_api(const char* name) override;

  sprockit::sim_parameters* params_;

 private:
  lib_compute_loops* compute_lib_;
  std::string unique_name_;

  int next_tls_key_;
  int next_condition_;
  int next_mutex_;

  std::map<long, thread*> subthreads_;
  std::map<int, mutex_t> mutexes_;
  std::map<int, condition_t> conditions_;
  std::map<int, destructor_fxn> tls_key_fxns_;
  std::map<std::string, api*> apis_;

  char* globals_storage_;

};

class user_app_cxx_full_main : public app
{
  FactoryRegister("user_app_cxx_full_main", app, user_app_cxx_full_main)
 public:
  user_app_cxx_full_main(sprockit::sim_parameters* params, software_id sid,
                         operating_system* os);

  static void register_main_fxn(const char* name, app::main_fxn fxn);

  void skeleton_main() override;

  static void delete_statics();

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
  FactoryRegister("user_app_cxx_empty_main", app, user_app_cxx_empty_main)
 public:
  user_app_cxx_empty_main(sprockit::sim_parameters* params, software_id sid,
                          operating_system* os);

  static void register_main_fxn(const char* name, app::empty_main_fxn fxn);

  void skeleton_main() override;

 private:
  static std::map<std::string, app::empty_main_fxn>* empty_main_fxns_;
  app::empty_main_fxn fxn_;

};

/** utility function for computing stuff */
void compute_time(double tsec);

}
} // end of namespace sstmac.

#endif