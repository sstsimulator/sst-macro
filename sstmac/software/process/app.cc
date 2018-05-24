/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h>

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/lib_compute_time.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/common/thread_lock.h>
#include <dlfcn.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/software/api/api.h>

static sprockit::need_delete_statics<sstmac::sw::user_app_cxx_full_main> del_app_statics;

RegisterKeywords(
 { "host_compute_timer", "whether to use the time elapsed on the host machine in compute modeling" },
 { "min_op_cutoff", "the minimum number of operations in a compute before detailed modeling is perfromed" },
 { "notify", "whether the app should send completion notifications to job root" },
 { "globals_size", "the size of the global variable segment to allocate" },
 { "OMP_NUM_THREADS", "environment variable for configuring openmp" },
);

MakeDebugSlot(app_compute);

namespace sstmac {
namespace sw {

std::map<std::string, app::main_fxn>*
  user_app_cxx_full_main::main_fxns_ = nullptr;
std::map<std::string, app::empty_main_fxn>*
  user_app_cxx_empty_main::empty_main_fxns_ = nullptr;
std::map<app_id, user_app_cxx_full_main::argv_entry> user_app_cxx_full_main::argv_map_;

int
app::allocate_tls_key(destructor_fxn fxn)
{
  int next = next_tls_key_;
  tls_key_fxns_[next] = fxn;
  ++next_tls_key_;
  return next;
}

static char* get_data_segment(sprockit::sim_parameters* params,
                              const char* param_name, GlobalVariableContext& ctx)
{
  int allocSize = ctx.allocSize();
  if (params->has_param(param_name)){
    allocSize = params->get_int_param(param_name);
    if (ctx.allocSize() != allocSize){
      ctx.setAllocSize(allocSize);
    }
  }
  if (allocSize != 0){
    char* segment = new char[allocSize];
    ::memcpy(segment, ctx.globalInit(), ctx.globalsSize());
    return segment;
  } else {
    return nullptr;
  }
}

char*
app::allocate_data_segment(bool tls)
{
  if (tls){
    return get_data_segment(params_, "tls_size", GlobalVariable::tlsCtx);
  } else {
    return get_data_segment(params_, "globals_size", GlobalVariable::glblCtx);
  }
}

app::app(sprockit::sim_parameters *params, software_id sid,
         operating_system* os) :
  thread(params, sid, os),
  compute_lib_(nullptr),
  params_(params),
  next_tls_key_(0),
  next_condition_(0),
  next_mutex_(0),
  min_op_cutoff_(0),
  globals_storage_(nullptr),
  omp_num_threads_(1),
  rc_(0)
{
  globals_storage_ = allocate_data_segment(false); //not tls
  min_op_cutoff_ = params->get_optional_long_param("min_op_cutoff", 1e3);
  bool host_compute = params->get_optional_bool_param("host_compute_timer", false);
  if (host_compute){
    host_timer_ = new HostTimer;
  }



  notify_ = params->get_optional_bool_param("notify", true);

  sprockit::sim_parameters* env_params = params->get_optional_namespace("env");
  omp_num_threads_ = env_params->get_optional_int_param("OMP_NUM_THREADS", 1);
}

app::~app()
{
  /** These get deleted by unregister */
  //sprockit::delete_vals(apis_);
  if (compute_lib_) delete compute_lib_;
  if (globals_storage_) delete[] globals_storage_;
  //we own a unique copy
  if (params_) delete params_;
}

lib_compute_memmove*
app::compute_lib()
{
  if(!compute_lib_) {
    compute_lib_ = new lib_compute_memmove(params_, sid_, os_);
  }
  return compute_lib_;
}

void
app::delete_statics()
{
}

void
app::cleanup()
{
  //okay, the app is dying
  //it may be that we have subthreads that are still active
  //all of these subthreads must be cancelled and never start again
  for (auto& pair : subthreads_){
    pair.second->cancel();
  }
  subthreads_.clear();

  thread::cleanup();
}

void
app::sleep(timestamp time)
{
  compute_lib()->sleep(time);
}

void
app::compute(timestamp time)
{
  compute_lib()->compute(time);
}

void
app::compute_inst(compute_event* cmsg)
{
  compute_lib()->compute_inst(cmsg);
}

void
app::compute_loop(uint64_t num_loops,
  int nflops_per_loop,
  int nintops_per_loop,
  int bytes_per_loop)
{
  compute_lib()->lib_compute_inst::compute_loop(
          num_loops, nflops_per_loop, nintops_per_loop, bytes_per_loop);
}

void
app::compute_detailed(uint64_t flops, uint64_t nintops, uint64_t bytes, int nthread)
{
  static const uint64_t overflow = 18006744072479883520ull;
  if (flops > overflow || bytes > overflow){
    spkt_abort_printf("flops/byte counts for compute overflowed");
  }
  if ((flops+nintops) < min_op_cutoff_){
    return;
  }

  debug_printf(sprockit::dbg::app_compute,
               "Rank %d for app %d: detailed compute for flops=%" PRIu64 " intops=%" PRIu64 " bytes=%" PRIu64,
               sid_.task_, sid_.app_, flops, nintops, bytes);

  compute_lib()->compute_detailed(flops, nintops, bytes,
                                  //if no number of threads are given, use the default OpenMP cfg
                                  nthread == use_omp_num_threads ? omp_num_threads_ : nthread);
}

void
app::compute_block_read(uint64_t bytes)
{
  compute_lib()->read(bytes);
}

void
app::compute_block_write(uint64_t bytes)
{
  compute_lib()->write(bytes);
}

sprockit::sim_parameters*
app::get_params()
{
  return operating_system::current_thread()->parent_app()->params();
}

void
app::compute_block_memcpy(uint64_t bytes)
{
  compute_lib()->copy(bytes);
}

api*
app::_get_api(const char* name)
{
  // an underlying thread may have built this
  api* my_api = apis_[name];
  if (!my_api) {
    sprockit::sim_parameters* api_params = params_->get_optional_namespace(name);
    api* new_api = api::factory::get_value(name, api_params, sid_, os_);
    apis_[name] = new_api;
    return new_api;
  } else {
    return my_api;
  }
}

void
app::run()
{
  SSTMACBacktrace(main);
  os_->increment_app_refcount();
  end_api_call(); //this initializes things, "fake" api call at beginning
  rc_ = skeleton_main();
  //we are ending but perform the equivalent
  //to a start api call to flush any compute
  start_api_call();
  for (auto& pair : apis_){
    delete pair.second;
  }
  apis_.clear();

  //now we have to send a message to the job launcher to let it know we are done
  os_->decrement_app_refcount();
  //for now assume that the application has finished with a barrier - which is true of like everything
  if (sid_.task_ == 0 && notify_){
    int launch_root = os_->node()->launch_root();
    job_stop_event* lev = new job_stop_event(sid_.app_, unique_name_, launch_root, os_->addr());
    os_->execute_kernel(ami::COMM_PMI_SEND, lev);
  }
  task_mapping::remove_global_mapping(sid_.app_, unique_name_);
  thread_info::deregister_user_space_virtual_thread(stack_);
}

void
app::add_subthread(thread *thr)
{
  if (thr->thread_id() == thread::main_thread){
    thr->init_id();
  }
  subthreads_[thr->thread_id()] = thr;
}

thread*
app::get_subthread(uint32_t id)
{
  auto it = subthreads_.find(id);
  if (it==subthreads_.end()){
    spkt_throw_printf(sprockit::value_error,
      "unknown thread id %u",
      id);
  }
  return it->second;
}

void
app::remove_subthread(uint32_t id)
{
  subthreads_.erase(id);
}

void
app::remove_subthread(thread *thr)
{
  subthreads_.erase(thr->thread_id());
}

bool
app::erase_mutex(int id)
{
  std::map<int,mutex_t>::iterator it = mutexes_.find(id);
  if (it == mutexes_.end()){
    return false;
  } else {
    mutexes_.erase(id);
    return true;
  }
}

bool
app::erase_condition(int id)
{
  std::map<int,condition_t>::iterator it = conditions_.find(id);
  if (it == conditions_.end()){
    return false;
  } else {
    conditions_.erase(id);
    return true;
  }
}

mutex_t*
app::get_mutex(int id)
{
  std::map<int,mutex_t>::iterator it = mutexes_.find(id);
  if (it==mutexes_.end()){
    return 0;
  } else {
    return &it->second;
  }
}

int
app::allocate_mutex()
{
  int id = next_mutex_++;
  mutexes_[id]; //implicit make
  return id;
}

int
app::allocate_condition()
{
  int id = next_condition_++;
  conditions_[id]; //implicit make
  return id;
}

condition_t*
app::get_condition(int id)
{
  std::map<int,condition_t>::iterator it = conditions_.find(id);
  if (it==conditions_.end()){
    return 0;
  } else {
    return &it->second;
  }
}

void
user_app_cxx_full_main::delete_statics()
{
  for (auto& pair : argv_map_){
    argv_entry& entry = pair.second;
    char* main_buffer = entry.argv[0];
    delete[] main_buffer;
    delete[] entry.argv;
  }
  argv_map_.clear();
  if (main_fxns_) delete main_fxns_;
  main_fxns_ = nullptr;
}

user_app_cxx_full_main::user_app_cxx_full_main(sprockit::sim_parameters *params, software_id sid,
                                               operating_system* os) :
  app(params, sid, os)
{
  if (!main_fxns_) main_fxns_ = new std::map<std::string, main_fxn>;

  std::string name = params->get_param("name");
  std::map<std::string, main_fxn>::iterator it = main_fxns_->find(name);
  if (it == main_fxns_->end()){
    spkt_throw_printf(sprockit::value_error,
                     "no user app with the name %s registered",
                     name.c_str());
  }
  fxn_ = it->second;
}

void
user_app_cxx_full_main::register_main_fxn(const char *name, app::main_fxn fxn)
{
  if (!main_fxns_) main_fxns_ = new std::map<std::string, main_fxn>;

  (*main_fxns_)[name] = fxn;
  app::factory::register_alias("user_app_cxx_full_main", name);
}

void
user_app_cxx_full_main::init_argv(argv_entry& entry)
{
  std::string appname = params_->get_param("name");
  std::vector<std::string> argv_param_vec;
  argv_param_vec.push_back(appname);
  if (params_->has_param("argv")) {
    params_->get_vector_param("argv", argv_param_vec);
  }
  int argc = argv_param_vec.size();
  char* argv_buffer = new char[256 * argc];
  char* argv_buffer_ptr = argv_buffer;
  char** argv = new char*[argc+1];
  for (int i = 0; i < argc; ++i) {
    const std::string& src_str = argv_param_vec[i];
    ::strcpy(argv_buffer_ptr, src_str.c_str());
    argv[i] = argv_buffer_ptr;
    //increment pointer for next strcpy
    argv_buffer_ptr += src_str.size() + 1; //+1 for null terminator
  }
  argv[argc] = nullptr; //missing nullptr - Issue #269
  entry.argc = argc;
  entry.argv = argv;
}

int
user_app_cxx_full_main::skeleton_main()
{
  static thread_lock argv_lock;
  argv_lock.lock();
  argv_entry& entry = argv_map_[sid_.app_];
  if (entry.argv == 0){
    init_argv(entry);
  }
  argv_lock.unlock();
  return (*fxn_)(entry.argc, entry.argv);
}

user_app_cxx_empty_main::user_app_cxx_empty_main(sprockit::sim_parameters *params, software_id sid,
                                                 operating_system* os) :
  app(params, sid, os)
{
  if (!empty_main_fxns_)
    empty_main_fxns_ = new std::map<std::string, empty_main_fxn>;

  std::string name = params->get_param("name");
  std::map<std::string, empty_main_fxn>::iterator it = empty_main_fxns_->find(name);
  if (it == empty_main_fxns_->end()){
    spkt_throw_printf(sprockit::value_error,
                     "no user app with the name %s registered",
                     name.c_str());
  }
  fxn_ = it->second;
}

void
user_app_cxx_empty_main::register_main_fxn(const char *name, app::empty_main_fxn fxn)
{
  if (!empty_main_fxns_)
    empty_main_fxns_ = new std::map<std::string, empty_main_fxn>;

  (*empty_main_fxns_)[name] = fxn;
  app::factory::register_alias("user_app_cxx_empty_main", name);
}

int
user_app_cxx_empty_main::skeleton_main()
{
  return (*fxn_)();
}

void compute_time(double tsec)
{
  thread* t = operating_system::current_thread();
  app* a = safe_cast(app, t,
     "cannot cast current thread to app in compute_time function");
  a->compute(timestamp(tsec));
}

}
}
